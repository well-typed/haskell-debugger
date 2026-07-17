{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NondecreasingIndentation #-}

-- | TODO: This module should be called Launch.
module Development.Debug.Adapter.DAPDebuggee where

#if !MIN_VERSION_ghc(9,15,0)
-- no longer needs to be imported from here in 9.15
import GHC.Conc.Sync (labelThread)
#endif

import GHC.IO.Handle
import GHC.Stack.Annotation (annotateStackStringIO)
import System.Process
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Control.Monad.Trans
import System.IO
import Control.Monad.Catch
import Control.Exception (throwIO, IOException)
import Control.Concurrent
import Control.Concurrent.Async (concurrently_)
import Control.Monad
import Data.Functor.Contravariant

import Development.Debug.Adapter
import Colog.Core as Logger
import qualified Development.Debug.Adapter.Output as Output


import DAP
import Development.Debug.Adapter.Handles
import Development.Debug.Session.Setup
import Development.Debug.Adapter.Proxy
import Network.Socket (socketPort, close)
import GHC.Debugger.Debuggee as Debugger
import GHC.Debugger.Utils (forwardHandleToLogger)

data DAPDebuggee = DAPDebuggee
  { dapdInterpreterSettings :: InterpreterSettings
  , dapdWaitForDebuggee :: IO ()
  , dapdThreads :: [(DebugAdaptorCont () -> IO ()) -> IO ()]
  -- ^ Additional threads to register for this session depending on the process
  -- we're running through `runInTerminal` (see 'interpreterInit').
  , dapdAfterRegister :: DebugAdaptor ()
  -- ^ additional commands to run after registering the session.
  }


internalNoInTerminalDAPD :: Applicative f => f DAPDebuggee
internalNoInTerminalDAPD
  -- Not using the terminal proxy, but we still want to output our own
  -- stdout/err (from the internal interpreter) as console events.
  = do
    let interpSettings = InterpreterSettings
          { interpreterFlags = mkInternalInterpreterFlags
          , interpreterSetup = mkInternalInterpreterSetup
          }
    pure $
      DAPDebuggee
        interpSettings
        (pure ())
        [ stdoutCaptureThread Nothing, stderrCaptureThread Nothing ]
        (pure ())

externalNoInTerminalDAPD :: MonadIO f => FilePath -> f DAPDebuggee
externalNoInTerminalDAPD hdbProg = do
  iserv_handles <- liftIO newEmptyMVar
  let interpSettings = InterpreterSettings
        { interpreterFlags = mkExternalInterpreterFlags hdbProg
        , interpreterSetup = mkExternalInterpreterSubProcessSetup CreatePipe CreatePipe CreatePipe (putMVar iserv_handles)
        }
  pure $
    DAPDebuggee
      interpSettings
      (pure ())
      [\ withAdaptor -> fwdThread iserv_handles (LogAction $ withAdaptor . Output.stdout) (LogAction $ withAdaptor . Output.stderr)
      ]
      (pure ())
  where
    fwdThread iserv_handles logOut logErr = annotateStackStringIO "External interpreter forwarding parent thread" $ do
      (_, Just serv_out, Just serv_err, _) <- takeMVar iserv_handles
      concurrently_
        (annotateStackStringIO "External interpreter stderr forwarding" $ forwardHandleToLogger serv_err logErr)
        (annotateStackStringIO "External interpreter stdout forwarding" $ forwardHandleToLogger serv_out logOut)

externalInTerminalDAPD :: MonadIO m => FilePath -> m DAPDebuggee
externalInTerminalDAPD hdbProg
  -- No additional bookkeeping is needed in this case because GHC will
  -- naturally have to wait for the external interpreter in order to start execution
  = liftIO $ do
  -- We keep the socket open so we claim the port.
  bracketOnError openSocketAvailablePort Network.Socket.close $ \ sock -> do
  let
    interpSettings = InterpreterSettings
      { interpreterFlags = mkExternalInterpreterFlags hdbProg
      , interpreterSetup = mkExternalInterpreterFromIOSetup
          $ annotateStackStringIO "Waiting for an external interpreter run-in-terminal process"
          $ extInterpFromListeningSocket sock
      }
  extInterpPort <- liftIO $ socketPort sock
  pure $
    DAPDebuggee
    interpSettings
    (pure ())
    -- When session is killed the socket is closed too.
    [\ _ -> forever (threadDelay 100_000_000) `finally` Network.Socket.close sock]
    (sendRunInTerminalReverseRequest
      RunInTerminalRequestArguments
        { runInTerminalRequestArgumentsKind = Just RunInTerminalRequestArgumentsKindIntegrated
        , runInTerminalRequestArgumentsTitle = Nothing
        , runInTerminalRequestArgumentsCwd = ""
        , runInTerminalRequestArgumentsArgs =
            [T.pack hdbProg, "external-interpreter", "--port", T.pack (show extInterpPort)]
        , runInTerminalRequestArgumentsEnv = Nothing
        , runInTerminalRequestArgumentsArgsCanBeInterpretedByShell = False
        })

internalInTerminalDAPD :: LogAction IO DAPSessionLog -> FilePath -> Adaptor DebugAdaptorState r DAPDebuggee
internalInTerminalDAPD l hdbProg = do
    (syncProxyIn, syncProxyOut, syncProxyErr)
                <- liftIO $ (,,) <$> newChan <*> newChan <*> newChan
    proxyClientReady <- liftIO $ newEmptyMVar

    (serverPort, serverProxyThread) <- liftIO $
      mkServerSideHdbProxy (contramap RunProxyServerLog l)
        syncProxyIn syncProxyOut syncProxyErr proxyClientReady
    let interpSettings = InterpreterSettings
          { interpreterFlags = mkInternalInterpreterFlags
          , interpreterSetup = mkInternalInterpreterSetup
          }
        waitForDebuggee =
          -- Only start executing after proxy client connects succesfully (#95)
          takeMVar proxyClientReady
    pure $ DAPDebuggee
      interpSettings
      waitForDebuggee
      [ const serverProxyThread
      -- Setup capturing of the process' own stdout and forwarding of the process' own stdin,
      -- but only because we're using the internal interpreter!
      , stdinForwardThread  syncProxyIn
      , stdoutCaptureThread (Just syncProxyOut)
      , stderrCaptureThread (Just syncProxyErr)
      ]

      -- When using the internal interpreter and 'runInTerminal' is supported
      -- (the 'RunProxyInTerminal' case), we ask the DAP client to launch the
      -- `hdb proxy` attached to the user's terminal. The proxy forwards
      -- input/output from the user terminal to the debugger+debuggee shared process
      (sendRunProxyInTerminal hdbProg serverPort)

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------
type SessionId = T.Text
data DAPSessionLog
  = DAPSessionSetupLog (WithSeverity SessionSetupLog)
  | DAPDebuggerLog Debugger.DebuggerLog
  | RunProxyServerLog (WithSeverity T.Text)


--------------------------------------------------------------------------------
-- * Capturing stdout, stderr, and writing to self stdin
--------------------------------------------------------------------------------

-- | Hijack the current process stdin and forward to it the messages from the given channel
stdinForwardThread :: Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdinForwardThread syncIn _withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stdin Forward Thread"

  -- We need to hijack stdin to write to it

  -- 1. Create a new pipe from writeEnd->readEnd
  (readEnd, writeEnd) <- createPipe

  -- 2. Substitute the read-end of the pipe by stdin
  _ <- hDuplicateTo readEnd stdin
  hClose readEnd -- we'll never need to read from readEnd

  forever $ do
    i <- readChan syncIn
    -- 3. Write to write-end of the pipe
    BS.hPut writeEnd i >> hFlush writeEnd

-- | This thread captures stdout from the debuggee and sends it to the client.
-- NOTE, redirecting the stdout handle is a process-global operation. So this thread
-- will capture ANY stdout the debuggee emits. Therefore you should never directly
-- write to stdout, but always write to the appropiate handle.
stdoutCaptureThread :: Maybe (Chan BS.ByteString) -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdoutCaptureThread msyncOut withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stdout Capture Thread"
  withInterceptedStdout $ \_ interceptedStdout -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStdout
      case msyncOut of
        Nothing -> pure ()
        Just syncOut -> writeChan syncOut $ T.encodeUtf8 (line <> T.pack "\n")

      -- Always output to Debug Console
      catch
        (withAdaptor $ Output.stdout line)
        (\(_ :: IOException) ->
          throwIO (FailedToWriteToAdaptor line))

-- | Like 'stdoutCaptureThread' but for stderr
stderrCaptureThread :: Maybe (Chan BS.ByteString) -> (DebugAdaptorCont () -> IO ()) -> IO ()
stderrCaptureThread msyncErr withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stderr Capture Thread"
  withInterceptedStderr $ \_ interceptedStderr -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStderr
      case msyncErr of
        Nothing -> pure ()
        Just syncErr -> writeChan syncErr $ T.encodeUtf8 (line <> "\n")

      -- Always output to Debug Console
      catch
        (withAdaptor $ Output.stderr line)
        (\(_ :: IOException) ->
          throwIO (FailedToWriteToAdaptor line))

newtype FailedToWriteToAdaptor = FailedToWriteToAdaptor T.Text
instance Show FailedToWriteToAdaptor where
  show (FailedToWriteToAdaptor t) = "Failed to write to debug adapter: " ++ T.unpack t
instance Exception FailedToWriteToAdaptor
