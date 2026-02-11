{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass,
   DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards, ViewPatterns,
   DataKinds #-}
module Main where

import System.Process
import System.Environment
import Data.Maybe
import Data.IORef
import Text.Read
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Exception (uninterruptibleMask)
import Control.Exception.Backtrace

import DAP

import Development.Debug.Adapter.Init
import Development.Debug.Adapter.Breakpoints
import Development.Debug.Adapter.Stepping
import Development.Debug.Adapter.Stopped
import Development.Debug.Adapter.Evaluation
import Development.Debug.Adapter.ExceptionInfo
import Development.Debug.Adapter.Exit
import Development.Debug.Adapter.Handles
import Colog.Core

import Data.Time
import System.IO (hSetBuffering, BufferMode(LineBuffering), Handle, openFile, IOMode(ReadMode))
import qualified DAP.Log as DAP
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD
import Data.Functor.Contravariant

import qualified GHCi.Server as GHCi
import qualified GHCi.Signals as GHCi
import qualified GHCi.Utils as GHCi
import qualified GHCi.Message as GHCi

import GHC.Utils.Logger (defaultLogActionWithHandles)
import GHC.Debugger.Monad (DebuggerLog(..), RunDebuggerSettings(..))
import Development.Debug.Options (HdbOptions(..))
import Development.Debug.Options.Parser (parseHdbOptions)
import Development.Debug.Adapter
import Development.Debug.Adapter.Proxy
import Development.Debug.Interactive

--------------------------------------------------------------------------------

main :: IO ()
main = do
  setBacktraceMechanismState CostCentreBacktrace False
  setBacktraceMechanismState HasCallStackBacktrace True
  setBacktraceMechanismState IPEBacktrace False -- TODO: True, see #181

  allArgs <- getArgs
  hdbOpts <- case allArgs of
    [writeFd, readFd, "--external-interpreter"] ->
         -- Special case to detect --external-interpreter in the third
         -- position. If we could specify -opti options to put *before* the
         -- descriptors we could get rid of this.
         pure (HdbExternalInterpreter (read writeFd) (read readFd))
    _ -> parseHdbOptions
  case hdbOpts of
    HdbDAPServer{port, internalInterpreter} -> do
      config <- getConfig port
      redirectRealStdout internalInterpreter $ \realStdout -> do
        hSetBuffering realStdout LineBuffering
        l <- mainLogger hdbOpts.verbosity realStdout
        init_var <- liftIO (newIORef False{-not supported by default-})
        pid_var  <- liftIO (newIORef Nothing)
        ccon_var <- liftIO newEmptyMVar
        runDAPServerWithLogger (contramap DAPLibraryLog l) config
          (talk l init_var pid_var ccon_var internalInterpreter)
          (ack l pid_var)
    HdbCLI{..} -> do
        l <- mainLogger hdbOpts.verbosity stdout
        stdinStream <- case debuggeeStdin of
          Just fp -> UseHandle <$> System.IO.openFile fp ReadMode
          Nothing -> pure Inherit
        let runConf = RunDebuggerSettings
              { supportsANSIStyling = True -- todo: check!!
              , supportsANSIHyperlinks = False
              , preferInternalInterpreter = internalInterpreter
              , externalInterpreterStdinStream = stdinStream
              }
        runIDM (contramap InteractiveLog l) entryPoint entryFile entryArgs extraGhcArgs
          runConf debugInteractive
    HdbProxy{port} -> do
        l <- mainLogger hdbOpts.verbosity stdout
        runInTerminalHdbProxy (contramap RunProxyClientLog l) port
    HdbExternalInterpreter{writeFd, readFd} -> do
      inh  <- GHCi.readGhcHandle (show readFd)
      outh <- GHCi.readGhcHandle (show writeFd)
      GHCi.installSignalHandlers
      pipe <- GHCi.mkPipeFromHandles inh outh
      let verbose = False
      uninterruptibleMask $ GHCi.serv verbose hook pipe
      where hook = return -- empty hook
        -- we cannot allow any async exceptions while communicating, because
        -- we will lose sync in the protocol, hence uninterruptibleMask.
  where
    -- When using the internal interpreter in DAP mode, we can't write to
    -- stdout directly because there will also be a thread forwarding the
    -- debuggee stdout by capturing it from stdout (and we'd get into a loop
    -- trying to forward what we're writing).
    --
    -- The redirection we use requires hDuplicateTo which isn't supported on
    -- Windows (ghc#22146), so using the internal interpreter on Windows
    -- currently unsupported.
    --
    -- When using the external interpreter, the debuggee output is read from
    -- its process handle directly, so this is unnecessary.
    redirectRealStdout internalInterpreter k
      | internalInterpreter =
        withInterceptedStdoutForwarding
          (\interceptedOut -> T.hPutStrLn stderr ("[INTERCEPTED STDOUT] " <> interceptedOut))
          (\realStdout -> k realStdout)
      | otherwise = k stdout


-- | Fetch config from environment, fallback to sane defaults
getConfig :: Int -> IO ServerConfig
getConfig port = do
  let
    hostDefault = "0.0.0.0"
    portDefault = port
    capabilities = Capabilities
      { supportsConfigurationDoneRequest      = True
      , supportsFunctionBreakpoints           = True
      , supportsConditionalBreakpoints        = True
      , supportsHitConditionalBreakpoints     = True
      , supportsEvaluateForHovers             = False
      -- Exception Breakpoints:
      , exceptionBreakpointFilters            = [ defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "All exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_EXCEPTION
                                                  }
                                                , defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "Uncaught exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_ERROR
                                                  }
                                                ]
      , supportsStepBack                      = False
      , supportsSetVariable                   = False
      , supportsRestartFrame                  = False
      , supportsGotoTargetsRequest            = False
      , supportsStepInTargetsRequest          = False
      , supportsCompletionsRequest            = False
      , completionTriggerCharacters           = []
      , supportsModulesRequest                = False
      , additionalModuleColumns               = [ defaultColumnDescriptor
                                                  { columnDescriptorAttributeName = "Extra"
                                                  , columnDescriptorLabel = "Label"
                                                  }
                                                ]
      , supportedChecksumAlgorithms           = []
      , supportsRestartRequest                = False
      , supportsExceptionOptions              = True
      , supportsValueFormattingOptions        = True
      , supportsExceptionInfoRequest          = True
      , supportTerminateDebuggee              = True
      , supportSuspendDebuggee                = False
      , supportsDelayedStackTraceLoading      = False
      , supportsLoadedSourcesRequest          = False
      , supportsLogPoints                     = False
      , supportsTerminateThreadsRequest       = False
      , supportsSetExpression                 = False
      , supportsTerminateRequest              = False
      , supportsDataBreakpoints               = False
      , supportsReadMemoryRequest             = False
      , supportsWriteMemoryRequest            = False
      , supportsDisassembleRequest            = False
      , supportsCancelRequest                 = False
      -- Display which breakpoints are valid when user intends to set
      -- breakpoint on given line:
      , supportsBreakpointLocationsRequest    = True
      , supportsClipboardContext              = False
      , supportsSteppingGranularity           = False
      , supportsInstructionBreakpoints        = False
      , supportsExceptionFilterOptions        = False
      , supportsSingleThreadExecutionRequests = False
      }
  ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure capabilities
    <*> pure True

--------------------------------------------------------------------------------
-- * Talk
--------------------------------------------------------------------------------

-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
talk :: LogAction IO MainLog
     -> IORef Bool
     -- ^ Whether the client supports runInTerminal
     -> IORef (Maybe Int)
     -- ^ The PID of the runInTerminal proxy process
     -> MVar ()
     -- ^ A var to block on waiting for the proxy client to connect, if a proxy
     -- connection is expected. See #95.
     -> Bool
     -- ^ Prefer internal interpreter
     -> Command -> DebugAdaptor ()
--------------------------------------------------------------------------------
talk l support_rit_var _pid_var client_proxy_signal prefer_internal_interpreter = \ case
  CommandInitialize -> do
    InitializeRequestArguments{supportsRunInTerminalRequest} <- getArguments
    let runInTerminal = fromMaybe False supportsRunInTerminalRequest
    liftIO $ writeIORef support_rit_var runInTerminal
    sendInitializeResponse

    -- If runInTerminal is not supported by the client, signal readiness right away
    when (not runInTerminal) $
      liftIO $ putMVar client_proxy_signal ()
--------------------------------------------------------------------------------
  CommandLaunch -> do
    launch_args <- getArguments

    supportsRunInTerminalRequest <- liftIO $ readIORef support_rit_var

    merror <- runExceptT $
      initDebugger (contramap DAPLog l)
        supportsRunInTerminalRequest prefer_internal_interpreter
        launch_args
    case merror of
      Right () -> do
        sendLaunchResponse   -- ack
        sendInitializedEvent -- our debugger is only ready to be configured after it has launched the session

        -- Run the proxy in a separate terminal to accept stdin / forward stdout
        -- if it is supported
        when supportsRunInTerminalRequest $ do
          -- Run proxy thread, server side, and
          -- send the 'runInTerminal' request
          serverSideHdbProxy (contramap RunProxyServerLog l) client_proxy_signal

        liftLogIO l <& DAPLaunchLog (WithSeverity (T.pack "Debugger launched successfully.") Info)

      Left (InitFailed err) -> do
        sendErrorResponse (ErrorMessage (T.pack err)) Nothing
        exitCleanly Nothing
--------------------------------------------------------------------------------
  CommandAttach -> do
    sendErrorResponse (ErrorMessage (T.pack "hdb does not support \"attach\" mode yet")) Nothing
    exitCleanly Nothing
--------------------------------------------------------------------------------
  CommandBreakpointLocations       -> commandBreakpointLocations
  CommandSetBreakpoints            -> commandSetBreakpoints
  CommandSetFunctionBreakpoints    -> commandSetFunctionBreakpoints
  CommandSetExceptionBreakpoints   -> commandSetExceptionBreakpoints
  CommandExceptionInfo             -> commandExceptionInfo
  CommandSetDataBreakpoints        -> undefined
  CommandSetInstructionBreakpoints -> undefined
----------------------------------------------------------------------------
  CommandLoadedSources -> undefined
----------------------------------------------------------------------------
  CommandConfigurationDone -> do
    sendConfigurationDoneResponse
    -- now that it has been configured, start executing until it halts, then send an event

    -- wait for the proxy client to connect before starting the execution (#95)
    () <- liftIO $ takeMVar client_proxy_signal
    startExecution >>= handleEvalResult False
----------------------------------------------------------------------------
  CommandThreads    -> commandThreads
  CommandStackTrace -> commandStackTrace
  CommandScopes     -> commandScopes
  CommandVariables  -> commandVariables
----------------------------------------------------------------------------
  CommandContinue   -> commandContinue
----------------------------------------------------------------------------
  CommandNext       -> commandNext
----------------------------------------------------------------------------
  CommandStepIn     -> commandStepIn
  CommandStepOut    -> commandStepOut
----------------------------------------------------------------------------
  CommandEvaluate   -> commandEvaluate
----------------------------------------------------------------------------
  CommandTerminate  -> do
    commandTerminate
  CommandDisconnect -> commandDisconnect
----------------------------------------------------------------------------
  CommandModules -> sendModulesResponse (ModulesResponse [] Nothing)
  CommandSource -> undefined
  CommandPause -> pure () -- TODO
  (CustomCommand "mycustomcommand") -> undefined
  (CustomCommand "runInTerminal") -> do
    -- Ignore result of runInTerminal (reverse request) response.
    -- If it fails, we simply continue without that functionality.
    pure ()
  other -> do
    sendErrorResponse (ErrorMessage (T.pack ("Unsupported command: " <> show other))) Nothing
    exitCleanly Nothing
----------------------------------------------------------------------------
-- talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------

-- | Receive reverse request responses (such as runInTerminal response)
ack :: LogAction IO MainLog
    -> IORef (Maybe Int)
    -- ^ Reference to PID of runInTerminal proxy process running
    -> ReverseRequestResponse -> DebugAdaptorCont ()
ack l _ref rrr = case rrr.reverseRequestCommand of
  ReverseCommandRunInTerminal -> do
    when rrr.success $ do
      liftLogIO l <& DAPLaunchLog (WithSeverity (T.pack "RunInTerminal was successful") Info)
  _ -> pure ()

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

data MainLog
  = DAPLog DAPLog
  | InteractiveLog InteractiveLog
  | RunProxyServerLog (WithSeverity T.Text)
  | RunProxyClientLog (WithSeverity T.Text)
  | DAPLaunchLog (WithSeverity T.Text)
  | DAPLibraryLog DAP.DAPLog

-- | Given the severity threshold from which we start logging, create a base
-- logger for consuming the top-level debugger logs ('MainLog').
-- Outputs to given handle.
mainLogger :: Severity -> Handle -> IO (LogAction IO MainLog)
mainLogger threshold h = do
  l <- handleLogger h
  let
    logSessionLog (WithSeverity msg sev)
      | sev >= threshold =
        cmapM renderWithTimestamp l <& (renderSeverity sev <> T.pack (show msg))
      | otherwise = pure ()

    logDebuggerLog = \case
      DebuggerLog sev msg
        | sev >= threshold ->
          cmapM renderWithTimestamp l <&
            (renderSeverity sev <> T.pack (show msg))
      GHCLog logflags msg_class srcSpan msg ->
        defaultLogActionWithHandles h h logflags msg_class srcSpan msg
      LogDebuggeeOut out ->
        -- If we wanted, we could log the debuggee output differently if we are
        -- on the DAP debug mode vs, say, hdb.
        l <& out
      LogDebuggeeErr err -> l <& err
      _ -> pure ()

    defaultLog (WithSeverity msg sev)
      | sev >= threshold =
        cmapM renderWithTimestamp l <& (renderSeverity sev <> msg)
      | otherwise = pure ()

  pure $ LogAction $ \case
    DAPLog (DAPSessionSetupLog sessionLog)       -> logSessionLog sessionLog
    DAPLog (DAPDebuggerLog debuggerLog)          -> logDebuggerLog debuggerLog
    InteractiveLog (ISessionSetupLog sessionLog) -> logSessionLog sessionLog
    InteractiveLog (IDebuggerLog debuggerLog)    -> logDebuggerLog debuggerLog
    RunProxyServerLog sev_msg -> defaultLog sev_msg
    RunProxyClientLog sev_msg -> defaultLog sev_msg
    DAPLaunchLog sev_msg      -> defaultLog sev_msg
    DAPLibraryLog t ->
      l <& DAP.renderDAPLog t
  where
    renderSeverity :: Severity -> Text
    renderSeverity = \ case
      Debug -> "[DEBUG] "
      Info -> "[INFO] "
      Warning -> "[WARNING] "
      Error -> "[ERROR] "

    renderWithTimestamp :: Text -> IO Text
    renderWithTimestamp msg = do
      t <- getCurrentTime
      let timeStamp = utcTimeToText t
      pure $ "[" <> timeStamp <> "] " <> msg
      where
        utcTimeToText utcTime = T.pack $
          formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" utcTime
