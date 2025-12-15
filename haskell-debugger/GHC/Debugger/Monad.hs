{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Debugger.Monad where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.IORef
import Data.Maybe
import Prelude hiding (mod)
import System.IO
#ifdef MIN_VERSION_unix
import System.Posix.Signals
#endif
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty

import GHC
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Diagnostic
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Driver.Ppr
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Loader as GHC
import GHC.Types.Error
import GHC.Types.PkgQual
import GHC.Types.SourceError
import GHC.Types.SourceText
import GHC.Types.Unique.Supply as GHC
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary as GHC
import GHC.Unit.Types
import GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC
import qualified GHC.LanguageExtensions as LangExt

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Logger as Logger
import GHC.Debugger.Runtime.Term.Cache
import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Session
import GHC.Debugger.Session.Builtin
import GHC.Debugger.Runtime.Compile.Cache
import qualified GHC.Debugger.Breakpoint.Map as BM
import qualified GHC.Debugger.Runtime.Thread.Map as TM

import {-# SOURCE #-} GHC.Debugger.Runtime.Instances.Discover (RuntimeInstancesCache, emptyRuntimeInstancesCache)

-- | A debugger action.
newtype Debugger a = Debugger { unDebugger :: ReaderT DebuggerState GHC.Ghc a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , GHC.HasDynFlags, MonadReader DebuggerState )

-- | State required to run the debugger.
--
-- - Keep track of active breakpoints to easily unset them all.
data DebuggerState = DebuggerState
      { activeBreakpoints :: IORef (BM.BreakpointMap (BreakpointStatus, BreakpointKind))
        -- ^ Maps a 'InternalBreakpointId' in Trie representation (map of Module to map of Int) to the
        -- 'BreakpointStatus' it was activated with.

      , varReferences     :: IORef (IM.IntMap TermKey, TermKeyMap Int)
      -- ^ When we're stopped at a breakpoint, this maps variable reference to
      -- Terms to allow further inspection and forcing by reference.
      --
      -- This map is only valid while stopped in this context. After stepping
      -- or resuming evaluation in any available way, this map becomes invalid
      -- and should therefore be cleaned.
      --
      -- The TermKeyMap map is a reverse lookup map to find which references
      -- already exist for given names

      , rtinstancesCache :: IORef RuntimeInstancesCache
      -- ^ RuntimeInstancesCache

      , threadMap         :: IORef TM.ThreadMap
      -- ^ 'ThreadMap' for threads spawned by the debuggee

      , compCache         :: IORef CompCache
      -- ^ Cache loaded and compiled expressions.

      , genUniq           :: IORef Int
      -- ^ Generates unique ints

      , hsDbgViewUnitId   :: Maybe UnitId
      -- ^ The unit-id of the companion @haskell-debugger-view@ unit, used for
      -- user-defined and built-in custom debug visualisations of values (e.g.
      -- for Strings or IntMap).
      --
      -- If the user depends on @haskell-debugger-view@ in its transitive
      -- closure, then we should use that exact unit which was solved by Cabal.
      -- The built-in instances and additional instances be available for the
      -- 'DebugView' class found in that unit. We can find the exact unit of
      -- the module by looking for @haskell-debugger-view@ in the module graph.
      --
      -- If the user does not depend on @haskell-debugger-view@ in any way,
      -- then we create our own unit and try to load the
      -- @haskell-debugger-view@ modules directly into it. As long as loading
      -- succeeds, the 'DebugView' class from this custom unit can be used to
      -- find the built-in instances for types like @'String'@
      --
      -- If the user explicitly disabled custom views, use @Nothing@.

      , dbgLogger :: Recorder (WithSeverity DebuggerMonadLog)
      }

-- | Enabling/Disabling a breakpoint
data BreakpointStatus
      -- | Breakpoint is disabled
      --
      -- Note: this must be the first constructor s.t.
      --  @BreakpointDisabled < {BreakpointEnabled, BreakpointAfterCount}@
      = BreakpointDisabled
      -- | Breakpoint is enabled
      | BreakpointEnabled
      -- | Breakpoint is disabled the first N times and enabled afterwards
      | BreakpointAfterCount Int
      -- | Breakpoint is enabled when condition evaluates to true
      | BreakpointWhenCond String
      -- | Breakpoint is disabled the first N times the condition evaluates to
      -- true and enabled in the next time it is true
      | BreakpointAfterCountCond Int String
      deriving (Eq, Ord, Show)

instance Outputable BreakpointStatus where ppr = text . show

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Additional settings configuring the debugger
data RunDebuggerSettings = RunDebuggerSettings
      { supportsANSIStyling :: Bool
      , supportsANSIHyperlinks :: Bool
      }

-- | Run a 'Debugger' action on a session constructed from a given GHC invocation.
runDebugger :: Recorder (WithSeverity DebuggerMonadLog)
            -> Handle     -- ^ The handle to which GHC's output is logged. The debuggee output is not affected by this parameter.
            -> FilePath   -- ^ Cradle root directory
            -> FilePath   -- ^ Component root directory
            -> FilePath   -- ^ The libdir (given with -B as an arg)
            -> [String]   -- ^ The list of units included in the invocation
            -> [String]   -- ^ The full ghc invocation (as constructed by hie-bios flags)
            -> [String]   -- ^ The extra GHC arguments (as given by the user in @extraGhcArgs@)
            -> FilePath   -- ^ Path to the main function
            -> RunDebuggerSettings -- ^ Other debugger run settings
            -> Debugger a -- ^ 'Debugger' action to run on the session constructed from this invocation
            -> IO a
runDebugger l dbg_out rootDir compDir libdir units ghcInvocation' extraGhcArgs mainFp conf (Debugger action) = do
  let ghcInvocation = filter (\case ('-':'B':_) -> False; _ -> True) ghcInvocation'
  GHC.runGhc (Just libdir) $ do
#ifdef MIN_VERSION_unix
    -- Workaround #4162
    _ <- liftIO $ installHandler sigINT Default Nothing
#endif
    dflags0 <- GHC.getSessionDynFlags
    let dflags1 = dflags0
          { GHC.ghcMode = GHC.CompManager
          , GHC.ghcLink = GHC.LinkInMemory
          , GHC.verbosity = 1
          , GHC.canUseColor = conf.supportsANSIStyling
          , GHC.canUseErrorLinks = conf.supportsANSIHyperlinks
          }
          -- Default debugger settings
          `GHC.xopt_set` LangExt.TypeApplications
          `GHC.xopt_set` LangExt.PackageImports
          `GHC.xopt_set` LangExt.MagicHash -- needed for some of the expressions we compile
          `GHC.gopt_set` GHC.Opt_ImplicitImportQualified
          `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
          `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
          `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
          `GHC.gopt_set` GHC.Opt_InsertBreakpoints
          & setBytecodeBackend
          & enableByteCodeGeneration

    GHC.modifyLogger $
      -- Override the logger to output to the given handle
      GHC.pushLogHook $ const $ debuggerLoggerAction dbg_out

    dflags2 <- getLogger >>= \logger -> do
      -- Set the extra GHC arguments for ALL units by setting them early in
      -- dynflags. This is important to make sure unfoldings for interfaces
      -- loaded because of the built-in loaded classes (like
      -- GHC.Debugger.View.Class) behave the same as if they were loaded for
      -- the user program. Otherwise we may run into the problem which
      -- 3093efa27468fb2d31a617f6a0e4ff67a90f6623 tried to fix (but had to be
      -- reverted)
      (dflags2, fileish_args, warns)
        <- parseDynamicFlags logger dflags1 (map noLoc extraGhcArgs)
      liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)
      -- todo: consider fileish_args?
      forM_ fileish_args $ \fish_arg -> liftIO $ do
        logMsg logger MCOutput noSrcSpan $ text "Ignoring extraGhcArg which isn't a recognized flag:" <+> text (unLoc fish_arg)
        printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)
      return dflags2

    -- Set the session dynflags now to initialise the hsc_interp.
    _ <- GHC.setSessionDynFlags dflags2

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins

    GHC.getSessionDynFlags >>= \df -> liftIO $
      GHC.initUniqSupply (GHC.initialUnique df) (GHC.uniqueIncrement df)

    -- Discover the user-given flags and targets
    flagsAndTargets <- parseHomeUnitArguments mainFp compDir units ghcInvocation dflags2 rootDir

    -- Setup base HomeUnitGraph
    setupHomeUnitGraph (NonEmpty.toList flagsAndTargets)
    -- Downsweep user-given modules first
    mod_graph_base <- doDownsweep Nothing

    if_cache <- Just <$> liftIO newIfaceCache

    -- Try to find or load the built-in classes from `haskell-debugger-view`
    (hdv_uid, loadedBuiltinModNames) <- findHsDebuggerViewUnitId mod_graph_base >>= \case
      Nothing -> (hsDebuggerViewInMemoryUnitId,) <$> do

        -- Not imported by any module: no custom views. Therefore, the builtin
        -- ones haven't been loaded. In this case, we will load the package ourselves.

        -- Add the custom unit to the HUG
        let base_dep_uids = [uid | UnitNode _ uid <- mg_mss mod_graph_base]
        addInMemoryHsDebuggerViewUnit base_dep_uids =<< getDynFlags

        tryLoadHsDebuggerViewModule l if_cache (const False) debuggerViewClassModName debuggerViewClassContents
          >>= \case
            Failed -> do
              -- Failed to load base debugger-view module!
              logWith l Debug $ LogFailedToCompileDebugViewModule debuggerViewClassModName
              return []
            Succeeded -> (debuggerViewClassModName:) . concat <$> do

              forM debuggerViewInstancesMods $ \(modName, modContent, pkgName) -> do
                -- Don't try to load instances whose packages are not even in
                -- the module graph:
                if any ((pkgName `L.isPrefixOf`) . unitIdString) base_dep_uids then do
                  tryLoadHsDebuggerViewModule l if_cache
                      ((\case
                          -- Keep only "GHC.Debugger.View.Class", which is a dependency of all these.
                          GHC.TargetFile f _
                            -> f == "in-memory:" ++ moduleNameString debuggerViewClassModName
                          _ -> False) . GHC.targetId)
                      modName modContent >>= \case
                    Failed -> do
                      logWith l Info $ LogFailedToCompileDebugViewModule modName
                      return []
                    Succeeded -> do
                      return [modName]
                else do
                  logWith l Debug $ LogSkippingViewModuleNoPkg modName pkgName (map unitIdString base_dep_uids)
                  return []

      Just uid ->
        -- TODO: We assume for now that if you depended on
        -- @haskell-debugger-view@, then you also depend on all its transitive
        -- dependencies (containers, text, ...), thus can load all custom
        -- views. Hence all `debuggerViewBuiltinMods`. In the future, we
        -- may want to guard all dependencies behind cabal flags that the user
        -- can tweak when depending on `haskell-debugger-view`.
        return (uid, map fst debuggerViewBuiltinMods)

    -- Final load combining all base modules plus haskell-debugger-view ones that loaded successfully
    -- The targets which were successfully loaded have been set with `setTarget` (e.g. by setupHomeUnitGraph).
    final_mod_graph <- doDownsweep (Just mod_graph_base{-cached previous result-})
    success <- doLoad if_cache GHC.LoadAllTargets final_mod_graph
    when (GHC.failed success) $ liftIO $
      throwM DebuggerFailedToLoad

    -- Set interactive context to import all loaded modules
    let preludeImp = GHC.IIDecl . GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"
    -- dbgView should always be available, either because we manually loaded it
    -- or because it's in the transitive closure.
    let dbgViewImps
          -- Using in-memory hs-dbg-view. It's a home-unit, so refer to it directly
          | hdv_uid == hsDebuggerViewInMemoryUnitId
          = map (GHC.IIModule . mkModule (RealUnit (Definite hdv_uid))) loadedBuiltinModNames
          -- It's available in a unit in the transitive closure. Resolve it.
          | otherwise
          = map (\mn ->
              GHC.IIDecl (GHC.simpleImportDecl mn)
              { ideclPkgQual = RawPkgQual
                  StringLiteral
                    { sl_st = NoSourceText
                    , sl_fs = mkFastString (unitIdString hdv_uid)
                    , sl_tc = Nothing
                    }
              }) loadedBuiltinModNames

    mss <- getAllLoadedModules

    GHC.setContext
      (preludeImp :
        dbgViewImps ++
        map (GHC.IIModule . GHC.ms_mod) mss)

    runReaderT action =<< initialDebuggerState l (if loadedBuiltinModNames == [] then Nothing else Just hdv_uid)

--------------------------------------------------------------------------------

-- | Run downsweep on the currently set targets (see @hsc_targets@)
doDownsweep :: GhcMonad m
            => Maybe ModuleGraph -- ^ Re-use existing module graph which was already summarised
            -> m ModuleGraph -- ^ Module graph constructed from current set targets
doDownsweep reuse_mg = do
  hsc_env <- getSession
#if MIN_VERSION_ghc(9,15,0)
  let msg = batchMultiMsg hsc_env
#else
  let msg = batchMultiMsg
#endif
  (errs_base, mod_graph) <- liftIO $ downsweep hsc_env mkUnknownDiagnostic (Just msg) (maybe [] mgModSummaries reuse_mg) [] False
  when (not $ null errs_base) $ do
#if MIN_VERSION_ghc(9,15,0)
    sec <- initSourceErrorContext . hsc_dflags <$> getSession
    throwErrors sec (fmap GhcDriverMessage (unionManyMessages errs_base))
#else
    throwErrors (fmap GhcDriverMessage (unionManyMessages errs_base))
#endif
  return mod_graph

doLoad :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> ModuleGraph -> m SuccessFlag
doLoad if_cache how_much mg = do
#if MIN_VERSION_ghc(9,15,0)
  msg <- batchMultiMsg <$> getSession
#else
  let msg = batchMultiMsg
#endif
  load' if_cache how_much mkUnknownDiagnostic (Just msg) mg

-- | Returns @Just modName@ if the given module was successfully loaded
tryLoadHsDebuggerViewModule
  :: GhcMonad m
  => Recorder (WithSeverity DebuggerMonadLog)
  -> Maybe ModIfaceCache
  -> (GHC.Target -> Bool)
  -- ^ Predicate to determine which of the existing
  -- targets should be re-used when doing downsweep
  -- Should be as minimal as necessary (i.e. just DebugView class for the
  -- instances modules).
  -> ModuleName -> StringBuffer -> m SuccessFlag
tryLoadHsDebuggerViewModule l if_cache keepTarget modName modContents = do
  dflags <- getDynFlags
  -- Store existing targets to restore afterwards
  -- We want to use as little targets as possible to keep downsweep minimal+fast
  old_targets <- GHC.getTargets

  -- Also: temporarily disable the logger! We don't want to show the user these
  -- modules we're trying to load and compile.
  restore_logger <- GHC.getLogger
  GHC.modifyLogger $
    -- Emit it all as Debug-level logs
    GHC.pushLogHook $ const $ \_ _ _ sdoc ->
      logWith l Logger.Debug $ LogSDoc dflags sdoc

  -- Make the target
  dvcT <- liftIO $ makeInMemoryHsDebuggerViewTarget modName modContents

  -- Make mod_graph just for this target
  GHC.setTargets (dvcT:filter keepTarget old_targets)
  dvc_mod_graph <- doDownsweep Nothing

  -- And try to load it
  result <- doLoad if_cache (GHC.LoadUpTo [mkModule hsDebuggerViewInMemoryUnitId modName]) dvc_mod_graph

  -- Restore targets plus new one if success
  GHC.setTargets (old_targets ++ (if succeeded result then [dvcT] else []))

  -- Restore logger
  GHC.modifyLogger $
    GHC.pushLogHook (const $ putLogMsg restore_logger)


  return result

--------------------------------------------------------------------------------
-- * Finding Debugger View
--------------------------------------------------------------------------------

-- | Fetch the @haskell-debugger-view@ unit-id from the environment.
-- @Nothing@ means custom debugger views are disabled.
getHsDebuggerViewUid :: Debugger (Maybe UnitId)
getHsDebuggerViewUid = asks hsDbgViewUnitId

-- | Try to find the @haskell-debugger-view@ unit-id in the transitive closure,
-- or, otherwise, return the a custom unit for which we'll load the
-- @haskell-debugger-view@ modules in it (essentially preparing an in-memory
-- version of the library to find the built-in instances in).
--
-- See also comment on the @'hsDbgViewUnitId'@ field of @'DebuggerState'@
findHsDebuggerViewUnitId :: ModuleGraph -> GHC.Ghc (Maybe UnitId)
findHsDebuggerViewUnitId mod_graph = do

  -- Only looks at unit-nodes, this is not robust!
  -- TODO: Better lookup of unit-id
  let hskl_dbgr_vws =
        [ uid
        | UnitNode _deps uid <- mg_mss mod_graph
        , "haskell-debugger-view" `L.isPrefixOf` unitIdString uid
        ]

  case hskl_dbgr_vws of
    [hdv_uid] ->
      -- In transitive closure, use that one.
      return (Just hdv_uid)
    [] -> do
      return Nothing
    _  ->
      error "Multiple unit-ids found for haskell-debugger-view in the transitive closure?!"

--------------------------------------------------------------------------------
-- Variable references
--------------------------------------------------------------------------------

-- | Find a variable's associated Term and Name by reference ('Int')
lookupVarByReference :: Int -> Debugger (Maybe TermKey)
lookupVarByReference i = do
  ioref <- asks varReferences
  (rm, _) <- readIORef ioref & liftIO
  return $ IM.lookup i rm

-- | Finds or creates an integer var reference for the given 'TermKey'.
-- TODO: Arguably, this mapping should be part of the debug-adapter, and
-- haskell-debugger should deal in 'TermKey' terms only.
getVarReference :: TermKey -> Debugger Int
getVarReference key = do
  ioref     <- asks varReferences
  (rm, tkm) <- readIORef ioref & liftIO
  (i, tkm') <- case lookupTermKeyMap key tkm of
    Nothing -> do
      new_i <- freshInt
      return (new_i, insertTermKeyMap key new_i tkm)
    Just existing_i ->
      return (existing_i, tkm)
  let rm' = IM.insert i key rm
  writeIORef ioref (rm', tkm') & liftIO
  return i

-- | Whenever we run a request that continues execution from the current
-- suspended state, such as Next,Step,Continue, this function should be called
-- to delete the variable references that become invalid as we leave the
-- suspended state.
--
-- In particular, @'varReferences'@ is reset.
--
-- See also section "Lifetime of Objects References" in the DAP specification.
leaveSuspendedState :: Debugger ()
leaveSuspendedState = do
  ioref <- asks varReferences
  liftIO $ writeIORef ioref mempty

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Generate a new unique 'Int'
freshInt :: Debugger Int
freshInt = do
  ioref <- asks genUniq
  i <- readIORef ioref & liftIO
  let !i' = i+1
  writeIORef ioref i'  & liftIO
  return i

-- | Initialize a 'DebuggerState'
initialDebuggerState :: Recorder (WithSeverity DebuggerMonadLog) -> Maybe UnitId -> GHC.Ghc DebuggerState
initialDebuggerState l hsDbgViewUid =
  DebuggerState <$> liftIO (newIORef BM.empty)
                <*> liftIO (newIORef mempty)
                <*> liftIO (newIORef emptyRuntimeInstancesCache)
                <*> liftIO (newIORef TM.emptyThreadMap)
                <*> liftIO (newIORef emptyCompCache)
                <*> liftIO (newIORef 0)
                <*> pure hsDbgViewUid
                <*> pure l

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: GHC.Ghc a -> Debugger a
liftGhc = Debugger . ReaderT . const

data DebuggerFailedToLoad = DebuggerFailedToLoad
instance Exception DebuggerFailedToLoad
instance Show DebuggerFailedToLoad where
  show DebuggerFailedToLoad = "Failed to compile and load user project."

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Modules
--------------------------------------------------------------------------------

-- | List all loaded modules 'ModSummary's
getAllLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getAllLoadedModules =
  (GHC.mgModSummaries <$> GHC.getModuleGraph) >>=
    filterM (\ms -> GHC.isLoadedModule (ms_unitid ms) (ms_mod_name ms))

--------------------------------------------------------------------------------
-- * Forcing laziness
--------------------------------------------------------------------------------

-- | The depth determines how much of the runtime structure is traversed.
-- @obtainTerm@ and friends handle fetching arbitrarily nested data structures
-- so we only depth enough to get to the next level of subterms.
defaultDepth :: Int
defaultDepth =  2

-- | Evaluate a suspended Term to WHNF.
--
-- Used in @'getVariables'@ to reply to a variable introspection request.
seqTerm :: HscEnv -> Term -> IO Term
seqTerm hsc_env term = do
  let
    interp = hscInterp hsc_env
    unit_env = hsc_unit_env hsc_env
  case term of
    Suspension{val, ty} -> do
      r <- GHCi.seqHValue interp unit_env val
      () <- fromEvalResult r
      let
        forceThunks = False {- whether to force the thunk subterms -}
        forceDepth  = defaultDepth
      cvObtainTerm hsc_env forceDepth forceThunks ty val
    NewtypeWrap{wrapped_term} -> do
      wrapped_term' <- seqTerm hsc_env wrapped_term
      return term{wrapped_term=wrapped_term'}
    _ -> return term

-- | Evaluate a Term to NF
deepseqTerm :: HscEnv -> Term -> IO Term
deepseqTerm hsc_env t = case t of
  Suspension{}   -> do t' <- seqTerm hsc_env t
                       deepseqTerm hsc_env t'
  Term{subTerms} -> do subTerms' <- mapM (deepseqTerm hsc_env) subTerms
                       return t{subTerms = subTerms'}
  NewtypeWrap{wrapped_term}
                 -> do wrapped_term' <- deepseqTerm hsc_env wrapped_term
                       return t{wrapped_term = wrapped_term'}
  _              -> do seqTerm hsc_env t

--------------------------------------------------------------------------------
-- * Instances
--------------------------------------------------------------------------------

instance GHC.HasLogger Debugger where
  getLogger = liftGhc GHC.getLogger

instance GHC.GhcMonad Debugger where
  getSession = liftGhc GHC.getSession
  setSession s = liftGhc $ GHC.setSession s

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

-- | The logger action used to log GHC output
debuggerLoggerAction :: Handle -> GHC.LogAction
debuggerLoggerAction h a b c d = do
  hSetEncoding h utf8 -- GHC output uses utf8
  -- potentially use the `Recorder` here?
  defaultLogActionWithHandles h h a b c d

data DebuggerMonadLog
  = LogFailedToCompileDebugViewModule GHC.ModuleName
  | LogSkippingViewModuleNoPkg GHC.ModuleName String [String]
  | LogSDoc DynFlags SDoc

instance Pretty DebuggerMonadLog where
  pretty = \ case
    LogFailedToCompileDebugViewModule mn ->
      pretty $ "Failed to compile built-in " ++ moduleNameString mn ++ " module! Ignoring these custom debug views."
    LogSkippingViewModuleNoPkg mn pkg uids ->
      pretty $ "Skipping compilation of built-in " ++ moduleNameString mn ++ " module because package "
                ++ show pkg ++ " wasn't found in dependencies " ++ show uids
    LogSDoc dflags doc ->
      pretty $ showSDoc dflags doc

logSDoc :: Logger.Severity -> SDoc -> Debugger ()
logSDoc sev doc = do
  dflags <- getDynFlags
  l <- asks dbgLogger
  logWith l sev (LogSDoc dflags doc)

logAction :: Recorder (WithSeverity DebuggerMonadLog) -> DynFlags -> GHC.LogAction
logAction l dflags = \_ msg_class _ sdoc -> do
    logWith l (msgClassSeverity msg_class) $ LogSDoc dflags sdoc

msgClassSeverity :: MessageClass -> Logger.Severity
msgClassSeverity = \case
  MCOutput -> Info
  MCFatal -> Logger.Error
  MCInteractive -> Info
  MCDump -> Debug
  MCInfo -> Info
  MCDiagnostic sev _ _ -> ghcSevSeverity sev

ghcSevSeverity :: GHC.Severity -> Logger.Severity
ghcSevSeverity = \case
  SevIgnore -> Debug -- ?
  SevWarning -> Logger.Warning
  SevError -> Logger.Error
