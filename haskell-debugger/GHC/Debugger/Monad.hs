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
import System.Posix.Signals
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty

import GHC.Utils.Trace

import GHC
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Loader as GHC
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Types.Unique.Supply as GHC
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModSummary as GHC
import GHC.Unit.Types
import GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Term.Cache
import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Session
import GHC.Debugger.Session.Builtin
import qualified GHC.Debugger.Breakpoint.Map as BM

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

      , termCache         :: IORef TermCache
      -- ^ TermCache

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
runDebugger :: Handle     -- ^ The handle to which GHC's output is logged. The debuggee output is not affected by this parameter.
            -> FilePath   -- ^ Cradle root directory
            -> FilePath   -- ^ Component root directory
            -> FilePath   -- ^ The libdir (given with -B as an arg)
            -> [String]   -- ^ The list of units included in the invocation
            -> [String]   -- ^ The full ghc invocation (as constructed by hie-bios flags)
            -> FilePath   -- ^ Path to the main function
            -> RunDebuggerSettings -- ^ Other debugger run settings
            -> Debugger a -- ^ 'Debugger' action to run on the session constructed from this invocation
            -> IO a
runDebugger dbg_out rootDir compDir libdir units ghcInvocation' mainFp conf (Debugger action) = do
  let ghcInvocation = filter (\case ('-':'B':_) -> False; _ -> True) ghcInvocation'
  GHC.runGhc (Just libdir) $ do
    -- Workaround #4162
    _ <- liftIO $ installHandler sigINT Default Nothing
    dflags0 <- GHC.getSessionDynFlags
    let dflags1 = dflags0
          { GHC.ghcMode = GHC.CompManager
          , GHC.ghcLink = GHC.LinkInMemory
          , GHC.verbosity = 1
          , GHC.canUseColor = conf.supportsANSIStyling
          , GHC.canUseErrorLinks = conf.supportsANSIHyperlinks
          }
          -- Default GHCi settings
          `GHC.gopt_set` GHC.Opt_ImplicitImportQualified
          `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
          `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
          `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
          `GHC.gopt_set` GHC.Opt_InsertBreakpoints
          & setBytecodeBackend
          & enableByteCodeGeneration

    GHC.modifyLogger $
      -- Override the logger to output to the given handle
      GHC.pushLogHook (const $ debuggerLoggerAction dbg_out)

    -- Set the session dynflags now to initialise the hsc_interp.
    _ <- GHC.setSessionDynFlags dflags1

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins

    GHC.getSessionDynFlags >>= \df -> liftIO $
      GHC.initUniqSupply (GHC.initialUnique df) (GHC.uniqueIncrement df)

    -- Discover the user-given flags and targets
    flagsAndTargets <- parseHomeUnitArguments mainFp compDir units ghcInvocation dflags1 rootDir

    -- Setup base HomeUnitGraph
    setupHomeUnitGraph (NonEmpty.toList flagsAndTargets)

#if MIN_VERSION_ghc(9,15,0)
    msg <- batchMultiMsg <$> getSession
#else
    let msg = batchMultiMsg
#endif

    -- Get mod_graph for base HUG
    (errs_base, mod_graph_base) <- depanalE mkUnknownDiagnostic (Just msg) [] False

    when (not $ isEmptyMessages errs_base) $ do
#if MIN_VERSION_ghc(9,15,0)
      sec <- initSourceErrorContext . hsc_dflags <$> getSession
      throwErrors sec (fmap GhcDriverMessage errs_base)
#else
      throwErrors (fmap GhcDriverMessage errs_base)
#endif

    (hdv_uid, success) <-
      findHsDebuggerViewUnitId mod_graph_base >>= \case
        Nothing -> do
          -- Not imported by any module: no custom views. Therefore, the builtin
          -- ones haven't been loaded. In this case, we will load the package ourselves.

          -- Add in-memory haskell-debugger-view unit
          in_mem_hdv <- liftIO . makeInMemoryHsDebuggerViewUnit =<< getDynFlags
          -- Try again, with custom modules loaded
          setupHomeUnitGraph (NonEmpty.toList flagsAndTargets ++ [in_mem_hdv])
          (errs, mod_graph) <- depanalE mkUnknownDiagnostic (Just msg) [] False
          when (not $ isEmptyMessages errs) $ do
#if MIN_VERSION_ghc(9,15,0)
            sec <- initSourceErrorContext . hsc_dflags <$> getSession
            throwErrors sec (fmap GhcDriverMessage errs)
#else
            throwErrors (fmap GhcDriverMessage errs)
#endif

          -- Load only up to debugger-view modules
          load' noIfaceCache (GHC.LoadUpTo [mkModule hsDebuggerViewInMemoryUnitId debuggerViewClassModName])
                mkUnknownDiagnostic (Just msg) mod_graph >>= \case
            Failed -> (Nothing,) <$> do
              -- Failed to load debugger-view modules! Try again without the haskell-debugger-view modules
              logger <- getLogger
              liftIO $ logMsg logger MCInfo noSrcSpan $
                text "Failed to compile built-in DebugView modules! Ignoring custom debug views."
              load' noIfaceCache GHC.LoadAllTargets mkUnknownDiagnostic (Just msg) mod_graph_base
            Succeeded -> (Just hsDebuggerViewInMemoryUnitId,) <$> do
              -- It worked! Now load everything else
              load' noIfaceCache GHC.LoadAllTargets mkUnknownDiagnostic (Just msg) mod_graph

        Just hdv_uid -> (Just hdv_uid,) <$>
          -- haskell-debug-view is in module graph already, so just load it all.
          load' noIfaceCache GHC.LoadAllTargets mkUnknownDiagnostic (Just msg) mod_graph_base

    when (GHC.failed success) $ liftIO $
      throwM DebuggerFailedToLoad

    -- Set interactive context to import all loaded modules
    let preludeImp = GHC.IIDecl . GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"
    -- dbgView should always be available, either because we manually loaded it
    -- or because it's in the transitive closure.
    let dbgViewImp uid = (mkModule (RealUnit (Definite uid)) debuggerViewClassModName)
    mss <- getAllLoadedModules

    GHC.setContext
      (preludeImp :
        (case hdv_uid of Just uid -> [GHC.IIModule $ dbgViewImp uid]; _ -> []) ++
        map (GHC.IIModule . GHC.ms_mod) mss)

    runReaderT action =<< initialDebuggerState hdv_uid

-- | The logger action used to log GHC output
debuggerLoggerAction :: Handle -> LogAction
debuggerLoggerAction h a b c d = do
  hSetEncoding h utf8 -- GHC output uses utf8
  defaultLogActionWithHandles h h a b c d

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
initialDebuggerState :: Maybe UnitId -> GHC.Ghc DebuggerState
initialDebuggerState hsDbgViewUid =
  DebuggerState <$> liftIO (newIORef BM.empty)
                <*> liftIO (newIORef mempty)
                <*> liftIO (newIORef mempty)
                <*> liftIO (newIORef 0)
                <*> pure hsDbgViewUid

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: GHC.Ghc a -> Debugger a
liftGhc = Debugger . ReaderT . const

data DebuggerFailedToLoad = DebuggerFailedToLoad
instance Exception DebuggerFailedToLoad
instance Show DebuggerFailedToLoad where
  show DebuggerFailedToLoad = "Failed to compile and load user project."

--------------------------------------------------------------------------------

type Warning = SDoc

displayWarnings :: [Warning] -> Debugger ()
displayWarnings ws = do
  logger <- getLogger
  liftIO $ logMsg logger MCInfo noSrcSpan (vcat ws)

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

