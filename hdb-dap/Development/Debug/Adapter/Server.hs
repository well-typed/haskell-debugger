{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Development.Debug.Adapter.Server where


import System.Environment
import Data.Maybe
import Text.Read
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import DAP

import Development.Debug.Adapter.Init
import Development.Debug.Adapter.Breakpoints
import Development.Debug.Adapter.Stepping
import Development.Debug.Adapter.Stopped
import Development.Debug.Adapter.Evaluation
import Development.Debug.Adapter.ExceptionInfo
import Development.Debug.Adapter.Exit
import Development.Debug.Adapter.Exit.Helpers
import Colog.Core

import Data.Time
import qualified DAP.Log as DAP
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Contravariant


import Development.Debug.Adapter


import GHC.Debugger.Monad
import qualified GHC.Utils.Logger as GHC


-------------------------------------------------------------------------
-- * DAP lib config
-------------------------------------------------------------------------

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
      , supportTerminateDebuggee              = False -- for now, when debugger is disconnected, we always kill the debuggee
      , supportSuspendDebuggee                = False
      , supportsDelayedStackTraceLoading      = False
      , supportsLoadedSourcesRequest          = False
      , supportsLogPoints                     = True
      , supportsTerminateThreadsRequest       = False
      , supportsSetExpression                 = False
      , supportsTerminateRequest              = True
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
talk :: LogAction IO DAPLog
     -> DAPServerConf
     -> Bool
     -- ^ Prefer internal interpreter
     -> Command -> DebugAdaptor ()
--------------------------------------------------------------------------------
talk l servConf prefer_internal_interpreter = \ case
  CommandInitialize -> do
    sendInitializeResponse
--------------------------------------------------------------------------------
  CommandLaunch -> do
    launch_args <- getArguments

    clientCaps <- getClientCapabilities
#ifdef mingw32_HOST_OS
    -- On Windows, runInTerminal is currently unsupported
    -- See #199
    let runInTerminal = False
#else
    let runInTerminal = fromMaybe False $ supportsRunInTerminalRequest =<< clientCaps
#endif

    initDebugger (contramap DAPSessionLog l) servConf
      runInTerminal prefer_internal_interpreter
      launch_args

    sendLaunchResponse   -- ack
    sendInitializedEvent -- our debugger is only ready to be configured after it has launched the session

    liftLogIO l <& DAPLaunchLog (WithSeverity (T.pack "Debugger launched successfully.") Info)
--------------------------------------------------------------------------------
  CommandAttach -> do
    sendTerminatedEvent (TerminatedEvent False)
    destroyDebugSession
    sendError (ErrorMessage (T.pack "hdb does not support \"attach\" mode yet")) Nothing
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

    DAS{runInTerminalProc} <- getDebugSession
    case runInTerminalProc of
      RunProxyInTerminal{proxyClientReady} -> liftIO $ do
        -- Only start executing after proxy client connects succesfully (#95)
        takeMVar proxyClientReady
      _ ->
        pure ()

    -- Configuration is finished. Start executing until it halts.
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
  CommandTerminate  -> commandTerminate
  CommandDisconnect -> commandDisconnect
----------------------------------------------------------------------------
  CommandModules -> sendModulesResponse (ModulesResponse [] Nothing)
  CommandSource -> undefined
  CommandPause -> pure () -- TODO
  (CustomCommand "mycustomcommand") -> undefined
  other -> do
    terminateWithError ("Unsupported command: " <> show other)

-- | Receive reverse request responses (such as runInTerminal response)
ack :: LogAction IO DAPLog
    -> ReverseRequestResponse -> DebugAdaptorCont ()
ack l rrr = case rrr.reverseRequestCommand of
  ReverseCommandRunInTerminal -> do

    RunInTerminalResponse{} <- getReverseRequestResponseBody rrr

    -- TODO: keep track of body.shellProcessId to then kill the proxy when the
    -- session is terminated:
    -- [stdout] [127.0.0.1:54427][DEBUG][RECEIVED]
    --  {
    --      "body": {
    --          "shellProcessId": 2092
    --      },
    --      "command": "runInTerminal",
    --      "seq": 14,
    --      "success": true,
    --      "type": "response"
    --  }
    when rrr.success $ do
      liftLogIO l <& DAPLaunchLog (WithSeverity (T.pack "RunInTerminal was successful") Info)
  _ -> pure ()

runHDBServer :: LogAction IO DAPLog -> DAPServerConf -> IO ()
runHDBServer l servConf@DAPServerConf{ dapServerConfig = config } = do
  runDAPServerWithLogger (contramap DAPLibraryLog l) config
    (talk l servConf False)
    (ack l )

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

data DAPLog
  = DAPSessionLog DAPSessionLog
  | DAPLaunchLog (WithSeverity T.Text)
  | DAPLibraryLog DAP.DAPLog

logSessionLog :: Show a => LogAction IO Text -> Severity -> WithSeverity a -> IO ()
logSessionLog l threshold (WithSeverity msg sev)
      | sev >= threshold =
        cmapM renderWithTimestamp l <& (renderSeverity sev <> T.pack (show msg))
      | otherwise = pure ()

logDebuggerLog :: GHC.LogAction -> LogAction IO Text -> Severity -> DebuggerLog -> IO ()
logDebuggerLog logGhcLog l threshold = \case
      DebuggerLog sev msg
        | sev >= threshold ->
          cmapM renderWithTimestamp l <&
            (renderSeverity sev <> T.pack (show msg))
      GHCLog logflags msg_class srcSpan msg ->
        logGhcLog logflags msg_class srcSpan msg
      LogDebuggeeOut out ->
        -- If we wanted, we could log the debuggee output differently if we are
        -- on the DAP debug mode vs, say, hdb.
        l <& out
      LogDebuggeeErr err -> l <& err
      _ -> pure ()

defaultLog :: LogAction IO Text -> Severity -> WithSeverity Text -> IO ()
defaultLog l threshold (WithSeverity msg sev)
      | sev >= threshold =
        cmapM renderWithTimestamp l <& (renderSeverity sev <> msg)
      | otherwise = pure ()

-- | Main log action for the HDB DAP server. Takes a log action for ghc messages, a
-- Text log action for everything else, and a severity threshold.
logDAPLog :: GHC.LogAction -> LogAction IO Text -> Severity -> LogAction IO DAPLog
logDAPLog logGhcLog l threshold = LogAction $ \case
      DAPSessionLog (DAPSessionSetupLog sessionLog)       -> logSessionLog l threshold sessionLog
      DAPSessionLog (DAPDebuggerLog debuggerLog)          -> logDebuggerLog logGhcLog l threshold debuggerLog
      DAPSessionLog (RunProxyServerLog sev_msg) -> defaultLog l threshold sev_msg
      DAPLaunchLog sev_msg      -> defaultLog l threshold sev_msg
      DAPLibraryLog t | convert t.severity >= threshold ->
        l <& DAP.renderDAPLog t
        | otherwise -> pure ()
  where
    convert DAP.DEBUG = Debug
    convert DAP.INFO = Info
    convert DAP.WARN = Warning
    convert DAP.ERROR = Error

renderSeverity :: Severity -> Text
renderSeverity = \case
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
