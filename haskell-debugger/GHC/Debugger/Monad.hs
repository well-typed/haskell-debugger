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

module GHC.Debugger.Monad
  ( module GHC.Debugger.Monad
  , module GHC.Debugger.Monad.Type
  , module GHC.Debugger.Monad.Load
  )
where

import Control.Concurrent
import qualified Data.Foldable as Foldable
import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.Maybe
import Prelude hiding (mod)
#ifdef MIN_VERSION_unix
import System.Posix.Signals
#endif
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Conc.Sync as C

import GHC
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Logger
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env as GHC
import GHC.Driver.Monad
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Make
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Loader as GHC
import GHC.Runtime.Context as GHCi
import GHC.Types.Error
import GHC.Unit.Module.Graph
import GHC.Unit.Types
import qualified GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC
import qualified GHC.LanguageExtensions as LangExt

import GHC.Debugger.Session
import GHC.Debugger.Session.Builtin

import Colog.Core as Logger

import GHC.Stack.Annotation
import GHC.Unit.Home.Graph
import GHC.Debugger.Utils.Orphans () -- bring orphan instances to everything which uses `Debugger`
import GHC.Debugger.Debuggee
import GHC.Plugins (HasCallStack)
import Data.Bifunctor
import GHC.Debugger.Monad.Type
import GHC.Debugger.Monad.Load

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

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
runDebuggerAction l rootDir extraGhcArgs conf loadHomeUnit (Debugger action)
  = flip MC.finally cleanupInterp $
          -- See Note [Shutting down the external interpreter]
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

#if !MIN_VERSION_ghc(9,14,2)
      loadFFIInspect l buildWays
#endif

      -- Loaded later so it can depend on FFIInspect if needed.
      loadInternal l buildWays


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
#if MIN_VERSION_ghc(10,1,0)
            [ i { ideclImportList = Just (Exactly, []) }
            | i <- instancesOnly ]
#else
            [ i { ideclImportList = Just (Exactly, L noAnn []) }
            | i <- instancesOnly ]
#endif

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
      hscInterp <$> GHC.getSession >>= \interp -> runInternal $ do
        code <- compileExprRemote $ moduleNameString debuggerRuntimeInternalModName ++ ".setLineBuffering"
        liftIO $ evalIO interp code

      noPrint <- lookupNoPrintConstant
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

