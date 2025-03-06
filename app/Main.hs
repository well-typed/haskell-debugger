{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase #-}
{-

== Overview

Main launches 3 lightweight threads.
- One thread listens to requests and writes them to a `Chan` (C1=requests)
- Another reads from `Chan` (C2=replies) and sends replies
- The worker thread reads requests from (C1), executes (e.g. by interpreting the debuggee program until a break), and writes responses to (C2)

┌────────────────────────────────────┐          
│Main                                │          
└┬────────────────────┬─────────────┬┘          
┌▽──────────────────┐┌▽───────────┐┌▽──────────┐
│Listen for requests││Send replies││GHC session│
└───────────────────┘└────────────┘└───────────┘


== Configuration

Currently, there is no support for changing the configuration of `ghc-debugger` at runtime.
- The `ghc-debugger` specific options are passed as process arguments as per 'Settings'.
- The GHC-specific flags that determine how to compile/interpret the project
  are passed after a double dash (@--@) in the list of arguments as well.
  Don't include MODE flags (like --interpreter)!

== Notes

For now, debugging is only supported in the interpreter mode.
When this changes, the code can be revised to accommodate those capabilities.
-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import System.Environment (getArgs)

import Debugger.Interface

import qualified GHC
import qualified GHC.Platform.Ways as GHC
import qualified GHC.Driver.DynFlags as GHC
import qualified GHC.Driver.Config.Logger as GHC
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Types.Unique.Supply as GHC
import qualified GHC.Runtime.Loader as GHC
import qualified Data.List.NonEmpty as NE

#if MIN_VERSION_GLASGOW_HASKELL(9,13,0,0)
import qualified GHC.Settings as GHC
#endif

data Settings = Settings
      { --   logLevel?
        -- , force inspect?
        -- , context-modules?
        ghcInvocation :: [String]
      , libdir :: Maybe FilePath
      }

-- ROMES: No, this is no good. The logic for handling flags and setting up the
-- session correctly alone is almost enough to justify implementing
-- ghc-debugger as a ghc mode.

main :: IO ()
main = do
  ghcInvocationFlags <- getArgs

  requests <- newChan
  replies  <- newChan

  _ <- forkIO $ receiver requests
  _ <- forkIO $ sender replies
  _ <- forkIO $ debugger requests replies
                  (mkSettings ghcInvocationFlags)

  return ()

-- | Make 'Settings' from ghc invocation flags
mkSettings :: [String] -> Settings
mkSettings flags = Settings
  { ghcInvocation = flags
  , libdir = listToMaybe $ mapMaybe (\case '-':'B':dir -> Just dir; _ -> Nothing) flags
  }

-- | The main worker. Runs a GHC session which executes 'Request's received from
-- the given @'Chan' 'Request'@ and writes 'Response's to the @'Chan' 'Response'@ channel.
debugger :: Chan Request -> Chan Response -> Settings -> IO ()
debugger requests replies Settings{libdir, ghcInvocation} =
  GHC.runGhc libdir $ do

    dflags0 <- GHC.getSessionDynFlags

    let dflags1 = dflags0
          { GHC.ghcMode = GHC.CompManager
          , GHC.backend = GHC.interpreterBackend
          , GHC.ghcLink = GHC.LinkInMemory
          , GHC.verbosity = 1
          }
          -- Default GHCi settings
          `GHC.gopt_set` GHC.Opt_ImplicitImportQualified
          `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
          `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
          `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
          `GHC.gopt_set` GHC.Opt_InsertBreakpoints

    logger1 <- GHC.getLogger
    let logger2 = GHC.setLogFlags logger1 (GHC.initLogFlags dflags1)

          -- The rest of the arguments are "dynamic"
          -- Leftover ones are presumably files
    (dflags2, fileish_args, _dynamicFlagWarnings) <-
        GHC.parseDynamicFlags logger2 dflags1 (map (GHC.mkGeneralLocated "on ghc-debugger command arg") ghcInvocation)

    -- Force using dyn ways if the target RTS linker
    -- only supports dynamic code
    let dflags3
#if MIN_VERSION_GLASGOW_HASKELL(9,13,0,0)
          | GHC.sTargetRTSLinkerOnlySupportsSharedLibs $ GHC.settings dflags2
              = GHC.setDynamicNow $
                -- See checkOptions below, -fexternal-interpreter is
                -- required when using --interactive with a non-standard
                -- way (-prof, -static, or -dynamic).
                GHC.setGeneralFlag' GHC.Opt_ExternalInterpreter $
                -- Use .o for dynamic object, otherwise it gets dropped
                -- with "Warning: ignoring unrecognised input", see
                -- objish_suffixes
                dflags2 { GHC.dynObjectSuf_ = GHC.objectSuf dflags2 }
#endif
          | otherwise = dflags2
    
    -- What does it mean to support the external interpreter in ghc-debug?
    let dflags4
          | not (GHC.gopt GHC.Opt_ExternalInterpreter dflags3) =
              let platform = GHC.targetPlatform dflags3
                  dflags3a = dflags3 { GHC.targetWays_ = GHC.hostFullWays }
                  dflags3b = foldl GHC.gopt_set dflags3a
                           $ concatMap (GHC.wayGeneralFlags platform)
                                       GHC.hostFullWays
                  dflags3c = foldl GHC.gopt_unset dflags3b
                           $ concatMap (GHC.wayUnsetGeneralFlags platform)
                                       GHC.hostFullWays
              in dflags3c
          | otherwise = dflags3

    -- TODO:Handle errors somewhere here and report them as responses (`prettyPrintGhcErrors`)?

    let (dflags5, srcs, objs) = GHC.parseTargetFiles dflags4 (map GHC.unLoc fileish_args)

    -- we've finished manipulating the DynFlags, update the session
    _ <- GHC.setSessionDynFlags dflags5
    dflags6 <- GHC.getSessionDynFlags

    -- RIGHT... We also need plugin loading
    -- Perhaps we need too much of GHC.
    -- To what extent should `ghc-debugger` be done directly in GHC as a mode of operation?

    -- Must do this before loading plugins
    liftIO $ GHC.initUniqSupply (GHC.initialUnique dflags6) (GHC.uniqueIncrement dflags6)

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins
    hsc_env <- GHC.getSession
    logger <- GHC.getLogger
    
    hs_srcs <- case NE.nonEmpty units of
      Just ne_units -> do
        GHC.initMulti ne_units
      Nothing -> do
        case srcs of
          [] -> return []
          _  -> do
            s <- initMake srcs
            return $ map (uncurry (,Nothing,)) s
    interactiveUI defaultGhciSettings hs_srcs maybe_expr


    -- TODO: initLoaderState

    forever $ do
      req <- liftIO $ readChan requests
      resp <- case req of
        SetBreakpoint bp -> _
        DelBreakpoint bp -> _
        GetStacktrace -> _
        GetVariables -> _
        GetSource -> _
        DoEval -> _
        DoContinue -> _
        DoStepLocal -> _
        DoSingleStep -> _
      liftIO $ writeChan replies resp

