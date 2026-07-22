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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NondecreasingIndentation #-}

module GHC.Debugger.Monad where

import Control.Concurrent
import Control.Exception
import qualified Data.Foldable as Foldable
import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import Data.Version (makeVersion, showVersion)
import Prelude hiding (mod)
#ifdef MIN_VERSION_unix
import System.Posix.Signals
#endif
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Conc.Sync as C

import GHC
import GHC.Data.StringBuffer
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Logger
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env as GHC
import GHC.Driver.Monad
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Driver.Ppr
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Loader as GHC
import GHC.Runtime.Context as GHCi
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Unit.Module.Graph
import GHC.Unit.State
import GHC.Unit.Types
import qualified GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC
import qualified GHC.LanguageExtensions as LangExt

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Session
import GHC.Debugger.Session.Builtin
import GHC.Debugger.Session.Interactive
import GHC.Debugger.Runtime.Compile.Cache
import qualified GHC.Debugger.Breakpoint.Map as BM
import qualified GHC.Debugger.Runtime.Thread.Map as TM

import Colog.Core as Logger

import {-# SOURCE #-} GHC.Debugger.Runtime.Instances.Discover (RuntimeInstancesCache, emptyRuntimeInstancesCache)
import GHC.Stack.Annotation
import GHC.Platform.Ways
import GHC.Unit.Home.Graph
import GHC.Debugger.Utils.Orphans () -- bring orphan instances to everything which uses `Debugger`
import System.Directory (getCurrentDirectory)
import GHC.Debugger.Debuggee
import GHC.Plugins (HasCallStack)
import Data.Bifunctor
import qualified GHC.Unit.Module.Graph as GHC

-- | A debugger action.
newtype Debugger a = Debugger { unDebugger :: ReaderT DebuggerState GHC.Ghc a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , GHC.HasDynFlags, MonadReader DebuggerState )

data BreakpointInfo = BreakpointInfo
  { bpInfoStatus :: !BreakpointStatus
  , bpInfoKind   :: !BreakpointKind
  , bpInfoAction :: !BreakpointAction
  }
  deriving (Eq,Show)

-- | State required to run the debugger.
--
-- - Keep track of active breakpoints to easily unset them all.
data DebuggerState = DebuggerState
      { activeBreakpoints :: IORef (BM.BreakpointMap BreakpointInfo)
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

-- | What to do when a breakpoint is enabled
data BreakpointAction
      -- | Evaluation is stopped, typical behaviour
      = BreakpointStop
      {- | A log message is printed and then evaluation resumes.
        The @String@ is an expression that takes care of interpolation and printing the log message.
      -}
      | BreakpointLogAndResume String
      deriving (Eq, Ord, Show)

instance Outputable BreakpointAction where ppr = text . show

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Additional settings configuring the debugger
data RunDebuggerSettings = RunDebuggerSettings
      { supportsANSIStyling :: Bool
      , supportsANSIHyperlinks :: Bool
      , interpreterSettings :: InterpreterSettings
      }

-- | Run a 'Debugger' action on a session constructed by a 'DebugRunner'
--
--  INVARIANT: The initUniqSupply has already been initialized.
--
--  Users of hdb-as-a-library will have to call `initUniqSupply` at their leisure,
--  special care needed if they supply any loaded units/modules to us via the DebugRunner,
--  as those will contain `Unique`s.
--
--  See Note [UniqueSupply is process global].
runDebugger :: LogAction IO DebuggerLog -> DebugRunner Ghc a -> RunDebuggerSettings -> Debugger a -> IO a
runDebugger l debugRunner conf action = annotateCallStackIO $ do
  debugRunner $ \ rootDir extraGhcArgs loadHomeUnit -> runDebuggerAction l rootDir extraGhcArgs conf loadHomeUnit action

type DebugSession m a
  =  FilePath -- ^ project root dir
  -> [String] -- ^ extra ghc args
  -> m ()   -- ^ action to load debugee home units
  -> Ghc a

type DebugRunner m a = DebugSession m a -> IO a

data ProjectDebugSpec = ProjectDebugSpec
      { rootDir :: FilePath
      -- ^ Project root directory
      , componentDir :: FilePath
      -- ^ Root dir of the loaded 'ComponentOptions'.
      -- Important for multi-package cabal projects, as packages are not in the
      -- root of the cradle, but in some sub-directory.
      , libdir :: FilePath
        -- ^ The libdir (given with -B as an arg)
      , units :: [String]
        -- ^ The list of units included in the invocation
      , ghcInvocation :: [String]
      -- ^ The full ghc invocation (as constructed by hie-bios flags)
      , absEntryFile :: FilePath
      -- ^ Path to the main function
      , extraGhcArgs :: [String]
      }

-- | Construct a session from paths and flags inferred from the debugee's project.
withProjectDebugSession
  :: GhcMonad m
  => ProjectDebugSpec
  -> DebugRunner m a
withProjectDebugSession ProjectDebugSpec{ghcInvocation = ghcI, ..} k = do
  let ghcInvocation = filter (\case ('-':'B':_) -> False; _ -> True) ghcI
  GHC.runGhc (Just libdir) $ do
#ifdef MIN_VERSION_unix
  -- Workaround #4162
  -- FIXME: setup reasonable handlers to run cleanupSession for every debugger thread, because runGhc's `withSignalHandlers` is not it.
    _ <- liftIO $ installHandler sigINT Default Nothing
    _ <- liftIO $ installHandler sigQUIT Default Nothing
    _ <- liftIO $ installHandler sigTERM Default Nothing
    _ <- liftIO $ installHandler sigHUP Default Nothing
#endif
    k rootDir extraGhcArgs $ do
    dflags2 <- getSessionDynFlags

    -- Discover the user-given flags and targets
    flagsAndTargets <- parseHomeUnitArguments absEntryFile componentDir units ghcInvocation dflags2 rootDir


    let setVerbosity dflags = dflags {verbosity = verbosity dflags2}
    -- Setup HomeUnitGraph with debugee and interactiveGhcDebugger units
    setupHomeUnitGraph (map (first setVerbosity) $ NonEmpty.toList flagsAndTargets)

    debugee_mod_graph <- doDownsweep Nothing

    if_cache <- Just <$> liftIO newIfaceCache
    success <- doLoad if_cache GHC.LoadAllTargets debugee_mod_graph

    when (GHC.failed success) $ liftIO $
      throwM DebuggerFailedToLoad

runDebuggerAction :: forall a. LogAction IO DebuggerLog
  -> FilePath -- ^ rootDir
  -> [String] -- ^ extraGhcArgs
  -> RunDebuggerSettings
  -> Ghc () -- ^ load home units action
  -> Debugger a
  -> Ghc a
runDebuggerAction l rootDir extraGhcArgs conf loadHomeUnit (Debugger action) = flip MC.finally cleanupInterp $ -- See Note [Shutting down the external interpreter]
  do
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

        & interpreterFlags conf.interpreterSettings
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
      <- parseDynamicFlagsWithRootDir rootDir logger dflags1 (map noLoc extraGhcArgs)
    liftIO $ printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)
    forM_ fileish_args $ \fish_arg -> liftIO $ do
      GHC.logMsg logger MCOutput noSrcSpan $ text "Ignoring extraGhcArg which isn't a recognized flag:" <+> text (unLoc fish_arg)
      printOrThrowDiagnostics logger (initPrintConfig dflags2) (initDiagOpts dflags2) (GhcDriverMessage <$> warns)
    return dflags2

  interpreterSetup conf.interpreterSettings l dflags2 $ do
      -- Initializes interpreter!
      _ <- GHC.setSessionDynFlags dflags2

      -- Initialise plugins here because the plugin author might already expect this
      -- subsequent call to `getLogger` to be affected by a plugin.
      GHC.initializeSessionPlugins

      preservingThreadLabel loadHomeUnit

      fixHomeUnitsDynFlagsForIIDecl


      -- Ensure all the home units are built with same Ways and return them.
      buildWays       <- do
        hug_dflags <- fmap homeUnitEnv_dflags . Foldable.toList . hsc_HUG <$> getSession
        liftIO $ validateUnitsWays $ case hug_dflags of
            [] -> error "No units"
            (x:xs) -> x NonEmpty.:| xs

#ifndef DEBUG_WITH_GHC
      -- Find haskell-debugger-view in (deps of) home units, or load one from
      -- in-memory sources.
      (hdv_uid, loadedBuiltinModNames) <- do
        preservingThreadLabel $
          findOrLoadHaskellDebuggerView l buildWays
#else
      let hdv_uid = hsDebuggerViewInMemoryUnitId
      let loadedBuiltinModNames = [] :: [ModuleName]
#endif

      -- See Note [Must explicitly expose module graph units]
      exposeModGraphUnitsInInteractiveGhcDebuggerUnit

      -- Set interactive context to import all loaded modules
      let preludeImp = GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"

      hsc_env_new <- getSession

      -- dbgView should always be available, either because we manually loaded it
      -- or because it's in the transitive closure.
      let dbgViewImps
            = map (packageImportDecl hvd_pkgName) loadedBuiltinModNames
            where
              hvd_pkgName = fromMaybe (error $ "No package name for: " ++ unitIdString hdv_uid) $
                lookupUnitPackageQualifier hsc_env_new hdv_uid

      mss <- getAllLoadedModules

      let
        imports
          = map GHC.IIDecl $ preludeImp :
            [ i { ideclImportList = Just (Exactly, L noAnn []) }
            | i <- instancesOnly ]

        -- We import (only the instances of) all the home unit
        -- modules to bring any orphan DebugView instances in scope.
        instancesOnly =
            dbgViewImps ++
            [ packageImportDecl pkgName (moduleName modl)
            | modl <- map moduleNodeInfoModule mss
            , let uid = moduleUnitId modl
            , let pkgName = fromMaybe (error $ "No package name for: " ++ unitIdString uid) $ lookupUnitPackageQualifier hsc_env_new uid
            ]


      GHC.setContext imports

      -- See Note [External interpreter buffering]
      setBufferings <- compileExprRemote """
        do { System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
            ; System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering }
        """

      -- FIXME: does this implicitly wait for the interpreter to be ready, or should we do so explicitly?
      hscInterp <$> GHC.getSession >>= \interp ->
        liftIO $ evalIO interp setBufferings

      noPrint <- defineNoPrint
      modifySession (\hsc_env -> hsc_env {hsc_IC = GHCi.setInteractivePrintName (hsc_IC hsc_env) noPrint})

      runReaderT action
        =<< initialDebuggerState (liftLogIO l)
            (if loadedBuiltinModNames == []
              then Nothing
              else Just hdv_uid)

preservingThreadLabel :: HasCallStack => Ghc a -> Ghc a
preservingThreadLabel m = do
  thId <- liftIO $ myThreadId
  mlbl <- liftIO $ C.threadLabel thId
  case mlbl of
    Nothing -> m
    Just lbl -> do
      annotateCallStackGhc $ do
        x <- m
        liftIO $ C.labelThread thId lbl
        pure x


findOrLoadHaskellDebuggerView :: LogAction IO DebuggerLog
             -> Ways
             -> Ghc (UnitId, [ModuleName])
findOrLoadHaskellDebuggerView l buildWays = do
  let ghcLog = liftLogIO l
  hsc_env <- getSession

  -- Try to find or load the built-in classes from `haskell-debugger-view`
  findHsDebuggerViewUnitId >>= \case
    Nothing -> (hsDebuggerViewInMemoryUnitId,) <$> do
      -- Not imported by any module: no custom views. Therefore, the builtin
      -- ones haven't been loaded. In this case, we will load the package ourselves.

      -- Add the custom unit to the HUG
      let base_dep_uids = graphsUnits hsc_env
      addInMemoryHsDebuggerViewUnit base_dep_uids . setDynFlagWays buildWays =<< getDynFlags

      -- Load unit modules using in-memory contents.
      let
        -- Don't try to load instances whose packages are not even in the
        -- module graph.
        (instanceMods,skipped) = L.partition (\ (_modName,_modContent,pkgName) -> any ((pkgName `L.isPrefixOf`) . unitIdString) base_dep_uids)
            debuggerViewInstancesMods
        modsToLoad =
          (debuggerViewClassModName,debuggerViewClassContents)
          : [ (modName,modContent)
            | (modName, modContent, _pkgName) <- instanceMods]

      forM_ skipped $ \(modName,_,pkgName) ->
        ghcLog <& DebuggerLog Logger.Debug
          (LogSkippingViewModuleNoPkg modName pkgName (map unitIdString base_dep_uids))

      successes <- loadInMemoryModules l hsDebuggerViewInMemoryUnitId modsToLoad

      fmap catMaybes . forM (zip successes modsToLoad) $ \case
        (Failed,(modName,_)) -> do
          ghcLog <& DebuggerLog Logger.Debug
            (LogFailedToCompileDebugViewModule modName)
          return $ Nothing
        (Succeeded,(modName,_)) ->
          return $ Just modName

    Just uid -> do
      -- TODO: We assume for now that if you depended on
      -- @haskell-debugger-view@, then you also depend on all its transitive
      -- dependencies (containers, text, ...), thus can load all custom
      -- views. Hence all `debuggerViewBuiltinMods`. In the future, we
      -- may want to guard all dependencies behind cabal flags that the user
      -- can tweak when depending on `haskell-debugger-view`.
      return (uid, map fst debuggerViewBuiltinMods)

{-
Note [Shutting down the external interpreter]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Ghc monad execution (under `runGhc`) sometimes terminates abruptly:
- When the DebugAdapter exits (e.g. disconnect, terminate, error, ...), it
  calls `destroyDebugSession`, which will *kill* the thread running the
  debugger/Ghc session.
- When there is some exception thrown in the Ghc monad itself

GHC wraps the `Ghc` action run with `withCleanupSession`, which is responsible
for e.g. removing temporary files and cleanly terminating the external
interpreter process, if one is being used.

GHC first checks with `getProcessExitCode` the status of the external
interpreter, does nothing if there is some exit status, and kills the external
process otherwise.

However, this check is incorrect(!) when the external interpreter process is
not a child of this process (which will happen in the runInTerminal external
interpreter case). `getProcessExitCode` should error with `ECHILD` in this case
(see `man 2 wait`), even if it doesn't yet (see process#359).

Therefore, the debugger must step in and make sure the external interpreter is
exited cleanly, WITHOUT resorting to `getProcessExitCode`. To this effect, we
add our own `MC.finally cleanupInterp` call which sends the `Shutdown` message
to the external interpreter before propagating the exception further (to GHC's
`withCleanupSession`, which will now do Nothing because we set `InterpPending`,
and beyond).

Note [Must explicitly expose module graph units]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`interactiveGhcDebugger` is our "current home unit", so its
`UnitState{moduleNameProvidersMap}` will determine which modules we can import
interactively (i.e. with GHC.setContext).

The `moduleNameProvidersMap` has so far only been required to expose, with
`ExposePackage` flags, the other home units. However, exposing a package **does
not** imply exposing its dependencies, so `mkUnitState` was free to choose
versions/abis for us, e.g., expose haskell-debugger-view-0.2.1.0-... and hide
haskell-debugger-view-0.2.0.0-..., while the latter is the one in the graph. We
noticed with `hdv` but the above can happen with any dep of the debuggee,
causing problems at the prompt.

Here we explicitly grab the units from the graph and make them exposed, so if we
find a unit in the graph we should be able to import exposed modules from it,
and importing modules at the prompt should use the versions the debuggee depends
on.

An alternative, closer to what ghci does, would be to copy the `packageFlags`
from the debuggee units, however doing so doesn't take care of fixing a unitId
for dependencies of dependencies.

Note [Package Qualified Imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Package qualified imports have a quirky behaviour: the source string qualifier gets converted into a `PkgQual` on the way, which can be one of these two:
- `ThisPkg unitId` interpreted as a home unit
- `OtherPkg unitId` interpreted as an external package

We get each under these conditions:
- ThisPkg
  - qualifier is the literal "this" or the **package name** of the active home unit or any of its home unit dependencies.
- OtherPkg
  - qualifier is the **package name** of a non-hidden external unit which exports the module we are importing.
  - None of the above apply, and the qualifier itself is interpreted as a `UnitId`.
When choosing between multiple units that satisfy a condition, the first found is committed to.

The upshot is that `UnitId`s normally only work as qualifiers for external packages, unless you change the package names of home units as described in  Note [ Ambiguous Package Qualified Imports Workaround ].

At the same time doing a PackageImport with a plain PackageName can succeed while resolving to an installed unit while we meant one of the loaded units, resulting in subtly wrong interactive sessions, where the
package-qualified imported module shadows the loaded module. Perhaps GHC could
warn about this. Cabal-repl and ghci also suffer from this subtle interaction.

In light of this, when the debugger imports the `haskell-debugger-view` modules,
it is imperative that if the `haskell-debugger-view` unit is in the home units
(e.g. if `haskell-debugger-view` is listed in the cabal.project, like it is in
the debugger tree), we rely on Note [ Ambiguous Package Qualified Imports Workaround ].

On the other hand, if the `haskell-debugger-view` package is not in the
home-units, we *should* package-qualify it to make sure we reference the right
one.

See also #283
-}

-- | See Note [Shutting down the external interpreter]
cleanupInterp :: Ghc ()
cleanupInterp = do
  interp <- hscInterp <$> getSession
  case interpInstance interp of
    InternalInterp -> pure ()
    ExternalInterp ext -> liftIO $ withExtInterpStatus ext $ \mstate -> do
      MC.mask $ \_restore -> modifyMVar_ mstate $ \state -> do
        case state of
          InterpPending    -> pure state -- already stopped
          InterpRunning i  -> do
            -- Can't use  `getProcessExitCode` because the interp process is
            -- not necessarily a child of this process (runInTerminal case).
            -- Just unconditionally try to send the message.
            sendMessage i Shutdown
            pure InterpPending

annotateDebuggerStackString :: String -> Debugger a -> Debugger a
annotateDebuggerStackString s (Debugger m) = Debugger $ do
  r <- ReaderT $ \val -> do
    withUnliftGhc $ \ unlift ->
      annotateStackStringIO s (unlift $ runReaderT m val)
  pure r

-- | Variant of GHC's parseDynamicFlags which interprets paths relative to first arg.
parseDynamicFlagsWithRootDir
    :: MonadIO m
    => FilePath
    -> Logger
    -> DynFlags
    -> [Located String]
    -> m (DynFlags, [Located String], Messages DriverMessage)
parseDynamicFlagsWithRootDir rootDir logger dflags cmdline = do
  (dflags1', leftovers, warns) <- parseDynamicFlagsCmdLine logger dflags cmdline
  -- flags that have just been read are used by the logger when loading package
  -- env
  let dflags1 = makeDynFlagsAbsoluteOverall rootDir dflags1'
  let logger1 = GHC.setLogFlags logger (initLogFlags dflags1)
  dflags2 <- liftIO $ interpretPackageEnv logger1 dflags1
  return (dflags2, leftovers, warns)


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

When launching the external interpreter directly attached to the user's
terminal (via runInTerminal), the handles will indeed be connected to a TTY.

[1] https://ftp.gnu.org/old-gnu/Manuals/glibc-2.2.5/html_node/Buffering-Concepts.html

Note [Dynamic dependencies for dynamic debugger]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the external interpreter running the debuggee is a dynamically-linked
program compiled with -fPIC, it is of utmost importance that the libraries we
load (e.g. base, ghc-internal, etc) are ALSO compiled with -fPIC. Otherwise, we
end up with the same SIGILL scenario of Note [Dynamic Debuggee for dynamic debugger].

In #260, we battled with another SIGILL for over a week because of this.
Namely, we forgot to configure the external interpreter's
IServConfig.iservConfDynamic (and had hardcoded it to False!!).

When loading a package to the external interpreter, GHC will consult
`iservConfDynamic` on whether to LoadDLL (dynamic lib) or LoadArchive (static
archive). This setting must definitely match the way in which the external
interpreter was compiled (checked with `hostIsDynamic`, since the external
interpreter and the debugger, while not necessarily the same process, are the
same executable). Ditto for `iservConfProfiled` (with `hostIsProfiled`).

Note [UniqueSupply is process global]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The generation of `Unique`s is controlled by two global pointers declared in the
`ghc` package. The same two pointers are shared by all sessions, since the host
ghc library is only loaded once.

If the pointers get re-initialized while a session is active, that session might generate
the same Unique again and we randomly get panics about identifiers out of scope or
which do not match their expected type and so on.

GHC calls the initialization function in main, with a comment saying it should be done
before initializing plugins.

The only safe time to initialize is if there are no existing Uniques that are still relevant,
and since it's also cheap we do it right away in `main`, for the `cli` or `server` commands.

Contrary to ghc itself, this means we do not honor the `initialUnique` and `uniqueIncrement`
fields of DynFlags, but they seem to be there for testing anyway.

Users of hdb-as-a-library, e.g. using runHDBServer, will have to do the initialization themselves,
especially if they supply any loaded units/modules to us via the DebugRunner,
as those will contain `Unique`s.

-}
--------------------------------------------------------------------------------

-- | Run downsweep on the currently set targets (see @hsc_targets@)
doDownsweep :: GhcMonad m
            => Maybe ModuleGraph -- ^ Re-use existing module graph which was already summarised
            -> m ModuleGraph -- ^ Module graph constructed from current set targets
doDownsweep reuse_mg = do
  hsc_env <- getSession
  let msg = batchMultiMsg
  (errs_base, mod_graph) <- liftIO $
    downsweep
      hsc_env mkUnknownDiagnostic (Just msg)
      (maybe [] mgModSummaries reuse_mg)
#if MIN_VERSION_ghc(10,1,0)
      reuse_mg
#endif
      [] False
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
  let msg = batchMultiMsg
  load' if_cache how_much mkUnknownDiagnostic (Just msg) mg


loadInMemoryModules ::
  LogAction IO DebuggerLog
  -> UnitId
  -> [(ModuleName,StringBuffer)] -> Ghc [SuccessFlag]
loadInMemoryModules l uid ts = do
  tgts <- forM ts $  \(modName,modContents) ->
    liftIO $ makeInMemoryTarget uid modName modContents
  GHC.setTargets tgts
  mod_graph <- hsc_mod_graph <$> GHC.getSession
  dvc_mod_graph <- doDownsweep (Just mod_graph)
  let new_mod_graph
#if MIN_VERSION_ghc(10,1,0)
        -- new API allows extending an existing graph.
        = dvc_mod_graph
#else
        = mkModuleGraph $ mg_mss dvc_mod_graph ++ mg_mss mod_graph
#endif
  modifySession $ GHC.setModuleGraph new_mod_graph

  restore_logger <- GHC.getLogger
  dflags <- getSessionDynFlags
  GHC.modifyLogger $
    -- Emit it all as Debug-level debugger logs
    GHC.pushLogHook $ const $ \_ _ _ sdoc ->
      l <& DebuggerLog Logger.Debug (LogSDoc dflags sdoc)

  -- Might not make sense to keep going if the first fails, but we expect all of
  -- them to succeed, and it's not that many more modules.
  s <- forM tgts $ \ tgt -> compileModuleWithDepsInHpt tgt >>= \case
        Nothing -> pure Succeeded
        Just e -> do
          liftLogIO l <& DebuggerLog Logger.Debug (LogSDoc dflags $ text (show e))
          pure Failed

  -- Restore logger
  GHC.modifyLogger $
    GHC.pushLogHook (const $ GHC.putLogMsg restore_logger)

  return s

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
findHsDebuggerViewUnitId :: GHC.Ghc (Maybe UnitId)
findHsDebuggerViewUnitId = do
  hsc_env <- getSession
  let unitState = hsc_units hsc_env

  -- Note: linear in the module graph but only happens once.
  let potential_units = graphsUnits hsc_env
  -- Note: the intermediate set is expected to be small (<= 2).
  let hskl_dbgr_vws = Set.toList . Set.fromList $
        [ uid
        | uid <- potential_units
        , let uid_s = unitIdString uid
        , "haskell-debugger-view" `L.isPrefixOf` uid_s
            || "hskll-dbggr-vw" `L.isPrefixOf` uid_s
            || "haskell-debug_" `L.isPrefixOf` uid_s
        ]

      -- If the haskell-debugger-view is in the dependency graph, it must have
      -- one of the versions the debugger is known to support:
      supported_ranges -- [min, max(
        = [ (makeVersion [0, 2], makeVersion [0, 3]) ]

  case hskl_dbgr_vws of
    [hdv_uid] -> do
      -- In transitive closure, use that one.
      -- Check that the version is in supported range.
      case lookupUnit unitState (RealUnit (Definite hdv_uid)) of
        Just unitInfo -> do
          let version = unitPackageVersion unitInfo
          if any (\(l,h) -> l <= version && version < h) supported_ranges
            then return (Just hdv_uid)
            else throwM UnsupportedHsDbgViewVersion{supportedVersions=supported_ranges, actualVersion=version}
        Nothing
          | "inplace" `L.isSuffixOf` unitIdString hdv_uid
          -- will be built as a target later
          -> return (Just hdv_uid)
        Nothing ->
          error "Could not find unit info for haskell-debugger-view"
    [] -> do
      return Nothing
    _  -> do
      error $ "Multiple unit-ids found for haskell-debugger-view in the transitive closure?!" ++ showSDocUnsafe (withPprStyle (PprDump alwaysQualify) (ppr hskl_dbgr_vws))

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
  { supportedVersions :: [ (Version, Version) ]
  , actualVersion :: Version
  }
instance Exception UnsupportedHsDbgViewVersion
instance Show UnsupportedHsDbgViewVersion where
  show (UnsupportedHsDbgViewVersion supported actual) =
    "Cannot use unsupported haskell-debugger-view version found in the transitive closure: " ++ showVersion actual ++
    " (supported: " ++ L.intercalate ", " (map (\(l,h) -> showVersion l ++ " <= && < " ++ showVersion h) supported) ++ ")"

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
getAllLoadedModules :: GHC.GhcMonad m => m [GHC.ModuleNodeInfo]
getAllLoadedModules =
  (mgInfos . mg_mss <$> GHC.getModuleGraph) >>=
    filterM (\ms -> GHC.isLoadedModule (moduleNodeInfoUnitId ms) (moduleNodeInfoModuleName ms))
  where
    mgInfos xs = [ info | ModuleNode _ info <- xs ]

getAllLoadedModulesWithPaths :: GHC.GhcMonad m =>
  m [(AbsFilePath,GHC.ModuleNodeInfo)]
getAllLoadedModulesWithPaths = do
  ghcCwd <- mkAbsolute <$> liftIO getCurrentDirectory
  -- TODO: cache?
  map (\ m -> (absoluteSourcePath ghcCwd m, m)) <$> getAllLoadedModules
  where
    absoluteSourcePath :: AbsFilePath -> ModuleNodeInfo -> AbsFilePath
    absoluteSourcePath ghcCwdDir ms
      = ghcCwdDir /> (fromMaybe (error $ "missing source path: " ++ show (moduleNodeInfoModuleName ms)) $ ml_hs_file (moduleNodeInfoLocation ms))

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
#if MIN_VERSION_ghc(9,15,0)
      r <- GHCi.seqHValue interp unit_env (hsc_logger hsc_env) val
#else
      r <- GHCi.seqHValue interp unit_env val
#endif
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


logSDoc :: Logger.Severity -> SDoc -> Debugger ()
logSDoc sev doc = do
  dflags <- getDynFlags
  l <- asks dbgLogger
  l <& DebuggerLog sev (LogSDoc dflags doc)
