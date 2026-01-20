{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
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

import System.Environment
import System.Process
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.Functor.Contravariant
import Data.IORef
import Data.Maybe
import Data.Version (makeVersion, showVersion)
import Prelude hiding (mod)
#ifdef MIN_VERSION_unix
import System.Posix.Signals
#endif
import Data.Text (Text)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty

import GHC
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Diagnostic
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Driver.Hooks
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
import GHC.Unit.State
import GHC.Unit.Module.ModSummary as GHC
import GHC.Unit.Types
import qualified GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC
import qualified GHC.LanguageExtensions as LangExt

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Session
import GHC.Debugger.Session.Builtin
import GHC.Debugger.Runtime.Compile.Cache
import GHC.Debugger.Utils
import qualified GHC.Debugger.Breakpoint.Map as BM
import qualified GHC.Debugger.Runtime.Thread.Map as TM

import Colog.Core as Logger

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

      , rtinstancesCache  :: IORef RuntimeInstancesCache
      -- ^ RuntimeInstancesCache

      , threadMap         :: IORef TM.ThreadMap
      -- ^ 'ThreadMap' for threads spawned by the debuggee

      , compCache         :: IORef CompCache
      -- ^ Cache loaded and compiled expressions.

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

      , dbgLogger :: LogAction Debugger DebuggerLog
      -- ^ See Note [Debugger, debuggee, and DAP logs]
      }

instance GHC.HasLogger Debugger where
  getLogger = liftGhc GHC.getLogger

instance GHC.GhcMonad Debugger where
  getSession = liftGhc GHC.getSession
  setSession s = liftGhc $ GHC.setSession s

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
      , preferInternalInterpreter :: Bool
      , externalInterpreterStdinStream :: StdStream
      -- ^ How to determine the stdin of the external interpreter running the
      -- debuggee. If not using the external interpreter this field is unused.
      }

-- | Run a 'Debugger' action on a session constructed from a given GHC invocation.
runDebugger :: LogAction IO DebuggerLog
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
runDebugger l rootDir compDir libdir units ghcInvocation' extraGhcArgs mainFp conf (Debugger action) = do
  let ghcLog = liftLogIO l :: LogAction Ghc DebuggerLog
  let dbgLog = liftLogIO l :: LogAction Debugger DebuggerLog
  thisProg <- getExecutablePath
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

          -- Enable the external interpreter by default! See #169
          -- See Note [Custom external interpreter]
          & enableExternalInterpreter conf.preferInternalInterpreter
          -- Ext interp is the same program as this, with "--external-interpreter"
          -- (this is ignored on GHC 9.14, see Note [Custom external interpreter])
          & setPgmI thisProg
          -- ideally, we'd set "external-interpreter" *before* the file
          -- descriptors. since there's no way to do that yet, we just have
          -- some logic in main to detect [writefd, readfd, --external-interpreter]
          & addOptI "--external-interpreter"

          -- Really important to force -dynamic if host is dynamic
          -- See Note [Dynamic Debuggee for dynamic debugger]
          & enableDynamicDebuggee

          & setBytecodeBackend
          & enableByteCodeGeneration

    GHC.modifyLogger $
      -- Override the logger to output to the given handle
      GHC.pushLogHook $ const $ ghcLogAction l

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
      forM_ fileish_args $ \fish_arg -> liftIO $ do
        GHC.logMsg logger MCOutput noSrcSpan $ text "Ignoring extraGhcArg which isn't a recognized flag:" <+> text (unLoc fish_arg)
        printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)
      return dflags2

    -- Make sure to override the function which creates the external
    -- interpreter, because we need to keep track of the standard handles
    iserv_handles <- liftIO newEmptyMVar
    modifySession $ \h -> h
      { hsc_hooks = (hsc_hooks h)
          { createIservProcessHook = Just $ \cp -> do
              -- See Note [External interpreter buffering]
              (_, Just o, Just e, ph) <-
                createProcess cp
                  { std_in  = conf.externalInterpreterStdinStream
                  , std_out = CreatePipe
                  , std_err = CreatePipe
                  -- Override executable path
                  -- See Note [Custom external interpreter]
#if MIN_VERSION_ghc(9,15,0)
#else
                  , cmdspec = case cmdspec cp of
                      ShellCommand (words -> ws) -> ShellCommand $ unwords $ thisProg : drop 1 ws
                      RawCommand _fp args -> RawCommand thisProg args
#endif
                  }
              putMVar iserv_handles (o, e)
              return ph
          }
      }

    when (GHC.gopt GHC.Opt_ExternalInterpreter dflags2) $ liftIO $ void $ do
      -- The external interpreter is spawned lazily, so we block waiting for
      -- the handles to be available in a new thread.
      forkIO $ do
        (serv_out, serv_err) <- takeMVar iserv_handles
        _ <- forkIO $
          forwardHandleToLogger serv_err (contramap LogDebuggeeErr l)
        forwardHandleToLogger serv_out (contramap LogDebuggeeOut l)

    -- Initializes interpreter!
    _ <- GHC.setSessionDynFlags dflags2

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins

    GHC.getSessionDynFlags >>= \df -> liftIO $
      GHC.initUniqSupply (GHC.initialUnique df) (GHC.uniqueIncrement df)

    -- Discover the user-given flags and targets
    flagsAndTargets <- parseHomeUnitArguments mainFp compDir units ghcInvocation dflags2 rootDir
    buildWays       <- liftIO $ validateUnitsWays flagsAndTargets

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
        addInMemoryHsDebuggerViewUnit base_dep_uids . setDynFlagWays buildWays =<< getDynFlags

        tryLoadHsDebuggerViewModule l if_cache (const False) debuggerViewClassModName debuggerViewClassContents
          >>= \case
            Failed -> do
              -- Failed to load base debugger-view module!
              ghcLog <& DebuggerLog Logger.Debug
                (LogFailedToCompileDebugViewModule debuggerViewClassModName)
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
                      ghcLog <& DebuggerLog Logger.Info
                        (LogFailedToCompileDebugViewModule modName)
                      return []
                    Succeeded -> do
                      return [modName]
                else do
                  ghcLog <& DebuggerLog Logger.Debug
                    (LogSkippingViewModuleNoPkg modName pkgName (map unitIdString base_dep_uids))
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

    -- See Note [External interpreter buffering]
    setBufferings <- compileExprRemote """
      do { System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
         ; System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering }
      """
    hscInterp <$> GHC.getSession >>= \interp ->
      liftIO $ evalIO interp setBufferings

    runReaderT action
      =<< initialDebuggerState dbgLog
          (if loadedBuiltinModNames == []
            then Nothing
            else Just hdv_uid)

{-
Note [Custom external interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We compile a custom external interpreter server with custom commands which make
certain debugger operations possible in the remote process directly.
This allows us to avoid excessive `Term` parsing and remote execution.
(Note: we don't have custom commands just yet, but that is the vision)

The custom external interpreter is the same executable as the debugger but invoked as:

  hdb <write-fd> <read-fd> --external-interpreter

(Note: external-interpreter is not the first argument because all `-opti`s are
always inserted after the write-fd and read-fd.)

When setting up the debugger session, we essentially set by default:
  - Enable -fexternal-interpreter
  - Set -pgmi=hdb and -opti=--external-interpreter
This can be switched off by toggling `--internal-interpreter`

With GHC 9.14, we have to override the `createProcess` executable call because
of ghc's c94aaacd4c4 (GHC looks for a `-dyn` suffixed version of the custom
external `-pgmi`, in this case `hdb` (but we do not have an `hdb-dyn`).
In GHC 9.16 it is sufficient to specify the -pgmi.

We can't use the on-the-fly external interpreter from GHC 9.14 because it is
not compiled with -threaded (with 9.16 in principle could, but we really want
the custom commands)

Note: The custom external interpreter must be compiled with -fkeep-cafs!
Why that is necessary is described in the GHC source code.

Note [Dynamic Debuggee for dynamic debugger]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A really really important point is that the debuggee MUST be linked dynamically
if the debugger was compiled dynamically (checked with `hostIsDynamic`).

Not doing this resulted in days upon days of suffering caused by a SIGILL fault.

The bug surfaces when we load a non-PIC static object of the debuggee into a
debugger that was linked dynamically with PIC. The runtime object linker does
not handle this correctly and something goes very wrong with the relocated
debuggee code.

Notably, this bug didn't surface on macOS because the static objects are also
compiled with -fPIC, and it didn't show up when using the distributed iserv
executables because that has very few dyn-link-time dependencies and for some
reason that doesn't trigger the bug. Adding more unused package dependencies
was sufficient to re-trigger it on windows.

Therefore, we always use -dynamic for compiling and loading the debuggee if the
debugger is dynamic (`hostIsDynamic`).

On Windows, the debugger will be static and we'll resort to statically linking
the debuggee too. There won't be a PIC mismatch so this should work fine.

Note [External interpreter buffering]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we launch the external interpreter process, we create pipes for
stdin/stdout/stderr instead of inheriting the current process' handles.
This allows us to cleanly separate the debuggee output from the debugger
output, without needing to redirect handles or the like.

However, using a pipe instead of a handle connected to a TTY means that, by
default, the line buffering will be block based rather than line buffered.

> Newly opened streams are normally fully buffered, with one exception: a
  stream connected to an interactive device such as a terminal is initially
  line buffered.^[1]

We depend on line buffering to forward output from these handles to the
debugger output (see `forwardHandleToLogger`).

Therefore, after loading the modules, we evaluate on the remote process:

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

[1] https://ftp.gnu.org/old-gnu/Manuals/glibc-2.2.5/html_node/Buffering-Concepts.html
-}
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
  => LogAction IO DebuggerLog
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
    -- Emit it all as Debug-level debugger logs
    GHC.pushLogHook $ const $ \_ _ _ sdoc ->
      l <& DebuggerLog Logger.Debug (LogSDoc dflags sdoc)

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
    GHC.pushLogHook (const $ GHC.putLogMsg restore_logger)


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
  hsc_env <- getSession
  let unitState = hsc_units hsc_env

  -- Only looks at unit-nodes, this is not robust!
  -- TODO: Better lookup of unit-id
  let hskl_dbgr_vws =
        [ uid
        | UnitNode _deps uid <- mg_mss mod_graph
        , "haskell-debugger-view" `L.isPrefixOf` unitIdString uid
            || "hskll-dbggr-vw" `L.isPrefixOf` unitIdString uid
        ]

      -- If the haskell-debugger-view is in the dependency graph, it must have
      -- one of the versions the debugger is known to support:
      supported_versions
        = [ makeVersion [0, 2, 0, 0] ]

  case hskl_dbgr_vws of
    [hdv_uid] -> do
      -- In transitive closure, use that one.
      -- Check that the version is exactly 0.2.0.0
      case lookupUnit unitState (RealUnit (Definite hdv_uid)) of
        Just unitInfo -> do
          let version = unitPackageVersion unitInfo
          if version `elem` supported_versions
            then return (Just hdv_uid)
            else throwM UnsupportedHsDbgViewVersion{supportedVersions=supported_versions, actualVersion=version}
        Nothing ->
          error "Could not find unit info for haskell-debugger-view"
    [] -> do
      return Nothing
    _  ->
      error "Multiple unit-ids found for haskell-debugger-view in the transitive closure?!"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Initialize a 'DebuggerState'
initialDebuggerState :: LogAction Debugger DebuggerLog -> Maybe UnitId -> GHC.Ghc DebuggerState
initialDebuggerState l hsDbgViewUid =
  DebuggerState <$> liftIO (newIORef BM.empty)
                <*> liftIO (newIORef emptyRuntimeInstancesCache)
                <*> liftIO (newIORef TM.emptyThreadMap)
                <*> liftIO (newIORef emptyCompCache)
                <*> pure hsDbgViewUid
                <*> pure l

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: GHC.Ghc a -> Debugger a
liftGhc = Debugger . ReaderT . const

data DebuggerFailedToLoad = DebuggerFailedToLoad
instance Exception DebuggerFailedToLoad
instance Show DebuggerFailedToLoad where
  show DebuggerFailedToLoad = "Failed to compile and load user project."

data UnsupportedHsDbgViewVersion = UnsupportedHsDbgViewVersion
  { supportedVersions :: [ Version ]
  , actualVersion :: Version
  }
instance Exception UnsupportedHsDbgViewVersion
instance Show UnsupportedHsDbgViewVersion where
  show (UnsupportedHsDbgViewVersion supported actual) =
    "Cannot use unsupported haskell-debugger-view version found in the transitive closure: " ++ showVersion actual ++
    " (supported: " ++ L.intercalate ", " (map showVersion supported) ++ ")"

expectRight :: Exception e => Either e a -> Debugger a
expectRight s = case s of
  Left e -> do
    logSDoc Logger.Error (text $ displayException e)
    liftIO $ throwIO e
  Right a -> do
    pure a

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
-- * Logging
--------------------------------------------------------------------------------

-- | A debugger log. May include debuggee ouput.
data DebuggerLog
  = DebuggerLog !Logger.Severity !DebuggerMessage
  | GHCLog !GHC.LogFlags !MessageClass !SrcSpan !SDoc
  | LogDebuggeeOut !Text
  | LogDebuggeeErr !Text

-- | A debugger log message
data DebuggerMessage
  = LogSDoc !DynFlags !SDoc
  | LogFailedToCompileDebugViewModule !GHC.ModuleName
  | LogSkippingViewModuleNoPkg !GHC.ModuleName String [String]

instance Show DebuggerMessage where
  show = \ case
    LogFailedToCompileDebugViewModule mn ->
      "Failed to compile built-in " ++ moduleNameString mn ++ " module! Ignoring these custom debug views."
    LogSkippingViewModuleNoPkg mn pkg uids ->
      "Skipping compilation of built-in " ++ moduleNameString mn ++ " module because package "
          ++ show pkg ++ " wasn't found in dependencies " ++ show uids
    LogSDoc dflags doc -> showSDoc dflags doc

logSDoc :: Logger.Severity -> SDoc -> Debugger ()
logSDoc sev doc = do
  dflags <- getDynFlags
  l <- asks dbgLogger
  l <& DebuggerLog sev (LogSDoc dflags doc)

ghcLogAction :: LogAction IO DebuggerLog -> GHC.LogAction
ghcLogAction l = \logflags mclass srcSpan sdoc -> do
    liftLogIO l <& GHCLog logflags mclass srcSpan sdoc

msgClassSeverity :: MessageClass -> Logger.Severity
msgClassSeverity = \case
  MCOutput -> Info
  MCFatal -> Logger.Error
  MCInteractive -> Info
  MCDump -> Debug
  MCInfo -> Info
  MCDiagnostic SevIgnore _ _ -> Debug -- ?
  MCDiagnostic SevWarning _ _ -> Logger.Warning
  MCDiagnostic SevError _ _ -> Logger.Error
