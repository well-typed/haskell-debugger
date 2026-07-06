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
module GHC.Debugger.Debuggee where

import System.Process
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.Functor.Contravariant
import Data.Maybe
import Prelude hiding (mod)
import Data.Text (Text)
import Network.Socket hiding (Debug)
import System.Process.Internals (mkProcessHandle)
import Text.Read (readMaybe)

import GHC
import GHC.Driver.Env as GHC
import GHC.Driver.Monad
import GHC.Driver.Hooks
import GHC.Driver.Ppr
import GHC.Runtime.Interpreter as GHCi
import GHC.Types.Error
import qualified GHC.Utils.Logger as GHC

import GHC.Debugger.Session
import GHC.Debugger.Utils

import Colog.Core as Logger

import GHCi.Message (mkPipeFromHandles)
import System.IO (hGetLine, IOMode(..), openFile, Handle)
import qualified GHC.Linker.Loader as Loader
import GHC.Stack.Annotation
import GHC.Platform.Ways
#if MIN_VERSION_ghc(9,15,0)
import GHC.Data.FastString.Env (emptyFsEnv)
#endif
import GHC.Debugger.Utils.Orphans () -- bring orphan instances to everything which uses `Debugger`
import System.Environment (getExecutablePath)

data InterpreterSettings = InterpreterSettings
      { interpreterFlags :: DynFlags -> DynFlags
      , interpreterSetup :: forall a. LogAction IO DebuggerLog -> DynFlags -> Ghc a -> Ghc a
      }

mkInternalInterpreterFlags :: DynFlags -> DynFlags
mkExternalInterpreterFlags :: String -> DynFlags -> DynFlags
(mkInternalInterpreterFlags, mkExternalInterpreterFlags) = (mkInterpreterFlags True "", mkInterpreterFlags False)
  where
    mkInterpreterFlags :: Bool -> String -> DynFlags -> DynFlags
    mkInterpreterFlags preferInternalInterpreter externalInterpreterProg df = df
      -- Enable the external interpreter by default! See #169
      -- See Note [Custom external interpreter]
      & enableExternalInterpreter preferInternalInterpreter
      -- Ext interp is the same program as this, with "--external-interpreter"
      -- (this is ignored on GHC 9.14, see Note [Custom external interpreter])
      & setPgmI externalInterpreterProg
      -- ideally, we'd set "external-interpreter" *before* the file
      -- descriptors. since there's no way to do that yet, we just have
      -- some logic in main to detect [writefd, readfd, --external-interpreter]
      & addOptI "--external-interpreter"


mkInternalInterpreterSetup :: LogAction IO DebuggerLog -> DynFlags -> Ghc a -> Ghc a
mkInternalInterpreterSetup _ dflags mainGhcThread = do
  when (gopt Opt_ExternalInterpreter dflags) $ do
    throw $ InconsistentInterpreterFlags $ "Used ghc flag  -fexternal-interpreter together with --internal-interpreter haskell-debugger flag."
  mainGhcThread

newtype InconsistentInterpreterFlags = InconsistentInterpreterFlags String
instance Show InconsistentInterpreterFlags where
  show (InconsistentInterpreterFlags t) = "Interpreter flags are inconsistent: " ++ t
instance Exception InconsistentInterpreterFlags


mkExternalInterpreterFromIOSetup :: IO Interp -> LogAction IO DebuggerLog -> DynFlags -> Ghc a -> Ghc a
mkExternalInterpreterFromIOSetup m _l dflags mainGhcThread = do
  unless (gopt Opt_ExternalInterpreter dflags) $ do
    throw $ InconsistentInterpreterFlags $ "Used ghc flag  -fno-external-interpreter instead of --internal-interpreter haskell-debugger flag."
  -- we supply our custom external interpreter process, which is
  -- already running and connected to the user's terminal.
  extInterp <- liftIO
    $ annotateStackStringIO "Waiting for an external interpreter run-in-terminal process"
    $ m
  modifySession $ \h -> h
    { hsc_interp = Just extInterp -- set it directly!
    }

  -- Ext interp is running in user terminal, no need to forward output to logger
  mainGhcThread


mkExternalInterpreterFromStdInSetup :: StdStream -> LogAction IO DebuggerLog -> DynFlags -> Ghc a -> Ghc a
mkExternalInterpreterFromStdInSetup givenStdStream l dflags mainGhcThread = do
  unless (gopt Opt_ExternalInterpreter dflags) $ do
    throw $ InconsistentInterpreterFlags $ "Used ghc flag  -fno-external-interpreter instead of --internal-interpreter haskell-debugger flag."

  (putHandles,fwdThread) <- liftIO $ externalInterpFwdThread l
  -- Make sure to override the function which creates the external
  -- interpreter, because we need to keep track of the standard handles
  modifySession $ \h -> h
    { hsc_hooks = (hsc_hooks h)
        { createIservProcessHook = Just $ \cp -> do
            -- See Note [External interpreter buffering]
            (_, Just o, Just e, ph) <-
              createProcess cp
                { std_in  = givenStdStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                -- Override executable path
                -- See Note [Custom external interpreter]
#if MIN_VERSION_ghc(9,15,0)
#else
                , cmdspec = case cmdspec cp of
                    ShellCommand (words -> ws) -> ShellCommand $ unwords $ getPgmI dflags : drop 1 ws
                    RawCommand _fp args -> RawCommand (getPgmI dflags) args
#endif
                }
            putHandles (o, e)
            return ph
        }
    }
  let
  -- We launched the external interpreter as a child process, so forward its output to the logger.
  withUnliftGhc $ \ unlift -> annotateCallStackIO $ do
    withAsync (annotateCallStackIO $ void $ fwdThread) $ \ fwd_thr -> do
      liftIO $ annotateCallStackIO $ link fwd_thr
      annotateCallStackIO $ unlift mainGhcThread

externalInterpFwdThread :: LogAction IO DebuggerLog ->
    IO ((Handle, Handle) -> IO (), IO ())
externalInterpFwdThread l = do
  iserv_handles <- liftIO newEmptyMVar
  -- The external interpreter is spawned lazily, so we block waiting for
  -- the handles to be available in a new thread.
  let
    fwdThread = takeMVar iserv_handles >>= \ (serv_out, serv_err) ->
      concurrently_
        (forwardHandleToLogger serv_err (contramap LogDebuggeeErr l))
        (forwardHandleToLogger serv_out (contramap LogDebuggeeOut l))
  return (putMVar iserv_handles, fwdThread)

-- | Make an 'ExtInterpInstance' based on an external interpreter process that
-- was launched by the DAP client via 'runInTerminal'. The process sends its
-- own PID as the first line on the socket before the GHCi wire protocol
-- begins.
extInterpFromTerminalProcess :: Socket -> IO Interp
extInterpFromTerminalProcess sock0 = do
  port <- socketPort sock0
  putStrLn $ "Connected to " ++ show port
  Control.Exception.bracketOnError
    (accept sock0)
    (\ (sock,_) -> close sock0 >> close sock)
    (\ (sock,_) -> do
      bi_h <- socketToHandle sock ReadWriteMode

      pidLine <- annotateCallStackIO $ hGetLine bi_h

      pid <- case readMaybe pidLine :: Maybe Int of
        Just pid -> pure pid
        Nothing  -> fail $ "invalid external interpreter PID on socket: " ++ show pidLine
      ph <- mkProcessHandle (fromIntegral pid) False
      interpPipe <- mkPipeFromHandles bi_h bi_h
      lock <- newMVar ()
      let process = InterpProcess
                      { interpHandle = ph
                      , interpPipe
                      , interpLock   = lock
                      }

      pending_frees <- newMVar []
      let inst = ExtInterpInstance
            { instProcess           = process
            , instPendingFrees      = pending_frees
            , instExtra             = ()
            }
          conf = IServConfig
            { iservConfProgram  = "the process is already running, we should never need to run it again"
            , iservConfOpts     = []
              -- VERY IMPORTANT: See Note [Dynamic dependencies for dynamic debugger]
            , iservConfDynamic  = hostIsDynamic
            , iservConfProfiled = hostIsProfiled
            , iservConfHook     = Nothing -- it's already running!
            , iservConfTrace    = pure ()
            }

      lookup_cache <- mkInterpSymbolCache
      s            <- newMVar $ InterpRunning inst
      loader       <- Loader.uninitializedLoader
#if MIN_VERSION_ghc(9,15,0)
      fs_cache     <- newMVar emptyFsEnv
      return (Interp (ExternalInterp (ExtIServ (ExtInterpState conf s))) loader lookup_cache fs_cache)
#else
      return (Interp (ExternalInterp (ExtIServ (ExtInterpState conf s))) loader lookup_cache)
#endif
      )

mkCliInterpreterSettings :: Bool -> Maybe FilePath -> IO InterpreterSettings
mkCliInterpreterSettings internalInterpreter debuggeeStdin = do
  if internalInterpreter then pure $ InterpreterSettings { interpreterFlags  = mkInternalInterpreterFlags
    , interpreterSetup = mkInternalInterpreterSetup } else do
  stdinStream <- case debuggeeStdin of
    Just fp -> UseHandle <$> System.IO.openFile fp ReadMode
    Nothing -> pure Inherit
  -- the same program invoked with `external-interpreter` serves as the external interpreter
  thisProg <- getExecutablePath
  pure InterpreterSettings { interpreterFlags = mkExternalInterpreterFlags thisProg
    , interpreterSetup = mkExternalInterpreterFromStdInSetup stdinStream
    }

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
