{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass, DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards, ViewPatterns #-}
module Main where

import System.Environment
import System.Process
import Data.Maybe
import Data.Aeson
import Data.IORef
import Text.Read
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import DAP

import Development.Debug.Adapter.Init
import Development.Debug.Adapter.Breakpoints
import Development.Debug.Adapter.Stepping
import Development.Debug.Adapter.Stopped
import Development.Debug.Adapter.Evaluation
import Development.Debug.Adapter.Exit
import Development.Debug.Adapter.Handles
import GHC.Debugger.Logger
import Prettyprinter

import System.IO (hSetBuffering, BufferMode(LineBuffering))
import qualified DAP.Log as DAP
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD

import Development.Debug.Options (HdbOptions(..))
import Development.Debug.Options.Parser (parseHdbOptions)
import Development.Debug.Adapter
import Development.Debug.Adapter.Proxy
import Development.Debug.Interactive

--------------------------------------------------------------------------------

defaultStdoutForwardingAction :: T.Text -> IO ()
defaultStdoutForwardingAction line = do
  T.hPutStrLn stderr ("[INTERCEPTED STDOUT] " <> line)

main :: IO ()
main = do
  hdbOpts <- parseHdbOptions
  let
    timeStampLogger  = cmapIO renderWithTimestamp . fromCologAction
    loggerWithSev    = cmap renderPrettyWithSeverity
    loggerFinal opts = applyVerbosity opts.verbosity . loggerWithSev . timeStampLogger
  case hdbOpts of
    HdbDAPServer{port} -> do
      config <- getConfig port
      withInterceptedStdoutForwarding defaultStdoutForwardingAction $ \realStdout -> do
        hSetBuffering realStdout LineBuffering
        l <- handleLogger realStdout
        let dapLogger = cmap DAP.renderDAPLog $ timeStampLogger l
        let runLogger = loggerFinal hdbOpts l
        init_var <- liftIO (newIORef False{-not supported by default-})
        pid_var  <- liftIO (newIORef Nothing)
        runDAPServerWithLogger (toCologAction dapLogger) config
          (talk runLogger init_var pid_var)
          (ack runLogger pid_var)
    HdbCLI{..} -> do
        l <- handleLogger stdout
        let runLogger = cmapWithSev InteractiveLog $ loggerFinal hdbOpts l
        runIDM runLogger entryPoint entryFile entryArgs extraGhcArgs $
          debugInteractive runLogger
    HdbProxy{port} -> do
        l <- handleLogger stdout
        let runLogger = cmapWithSev RunProxyClientLog $ loggerFinal hdbOpts l
        runInTerminalHdbProxy runLogger port

-- | Fetch config from environment, fallback to sane defaults
getConfig :: Int -> IO ServerConfig
getConfig port = do
  let
    hostDefault = "0.0.0.0"
    portDefault = port
    capabilities = defaultCapabilities
      { -- Exception breakpoints!
        exceptionBreakpointFilters            = [ defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "All exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_EXCEPTION
                                                  }
                                                , defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "Uncaught exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_ERROR
                                                  }
                                                ]
        -- Function breakpoints!
      , supportsFunctionBreakpoints           = True

      , supportsEvaluateForHovers             = False

      -- display which breakpoints are valid when user intends to set
      -- breakpoint on given line.
      , supportsBreakpointLocationsRequest    = True
      , supportsConfigurationDoneRequest      = True
      , supportsHitConditionalBreakpoints     = False
      , supportsModulesRequest                = False
      , additionalModuleColumns               = [ defaultColumnDescriptor
                                                  { columnDescriptorAttributeName = "Extra"
                                                  , columnDescriptorLabel = "Label"
                                                  }
                                                ]
      , supportsValueFormattingOptions        = True
      , supportTerminateDebuggee              = True
      , supportsLoadedSourcesRequest          = False
      , supportsExceptionOptions              = True
      , supportsExceptionFilterOptions        = False
      }
  ServerConfig
    <$> do fromMaybe hostDefault <$> lookupEnv "DAP_HOST"
    <*> do fromMaybe portDefault . (readMaybe =<<) <$> do lookupEnv "DAP_PORT"
    <*> pure capabilities
    <*> pure True

--------------------------------------------------------------------------------
-- * Talk
--------------------------------------------------------------------------------

data MainLog
  = InitLog InitLog
  | LaunchLog T.Text
  | InteractiveLog InteractiveLog
  | RunProxyServerLog ProxyLog
  | RunProxyClientLog ProxyLog

instance Pretty MainLog where
  pretty = \ case
    InitLog msg -> pretty msg
    LaunchLog msg -> pretty msg
    InteractiveLog msg -> pretty msg
    RunProxyServerLog msg -> pretty ("Proxy Server:" :: String) <+> pretty msg
    RunProxyClientLog msg -> pretty ("Proxy Client:" :: String) <+> pretty msg

-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
talk :: Recorder (WithSeverity MainLog)
     -> IORef Bool
     -- ^ Whether the client supports runInTerminal
     -> IORef (Maybe Int)
     -- ^ The PID of the runInTerminal proxy process
     -> Command -> DebugAdaptor ()
--------------------------------------------------------------------------------
talk l support_rit_var pid_var = \ case
  CommandInitialize -> do
    InitializeRequestArguments{supportsRunInTerminalRequest} <- getArguments
    liftIO $ writeIORef support_rit_var supportsRunInTerminalRequest
    sendInitializeResponse

--------------------------------------------------------------------------------
  CommandLaunch -> do
    launch_args <- getArguments

    supportsRunInTerminalRequest <- liftIO $ readIORef support_rit_var

    merror <- runExceptT $ initDebugger (cmapWithSev InitLog l) supportsRunInTerminalRequest launch_args
    case merror of
      Right () -> do
        sendLaunchResponse   -- ack
        sendInitializedEvent -- our debugger is only ready to be configured after it has launched the session

        -- Run the proxy in a separate terminal to accept stdin / forward stdout
        -- if it is supported
        when supportsRunInTerminalRequest $ do
          -- Run proxy thread, server side, and
          -- send the 'runInTerminal' request
          serverSideHdbProxy (cmapWithSev RunProxyServerLog l)

        logWith l Info $ LaunchLog $ T.pack "Debugger launched successfully."

      Left (InitFailed err) -> do
        sendErrorResponse (ErrorMessage (T.pack err)) Nothing
        exitCleanly
--------------------------------------------------------------------------------
  CommandAttach -> do
    sendErrorResponse (ErrorMessage (T.pack "hdb does not support \"attach\" mode yet")) Nothing
    exitCleanly
--------------------------------------------------------------------------------
  CommandBreakpointLocations       -> commandBreakpointLocations
  CommandSetBreakpoints            -> commandSetBreakpoints
  CommandSetFunctionBreakpoints    -> commandSetFunctionBreakpoints
  CommandSetExceptionBreakpoints   -> commandSetExceptionBreakpoints
  CommandSetDataBreakpoints        -> undefined
  CommandSetInstructionBreakpoints -> undefined
----------------------------------------------------------------------------
  CommandLoadedSources -> undefined
----------------------------------------------------------------------------
  CommandConfigurationDone -> do
    sendConfigurationDoneResponse
    -- now that it has been configured, start executing until it halts, then send an event
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
  CommandPause -> undefined
  (CustomCommand "mycustomcommand") -> undefined
  (CustomCommand "runInTerminal") -> do
    -- Ignore result of runInTerminal (reverse request) response.
    -- If it fails, we simply continue without that functionality.
    pure ()
  other -> do
    sendErrorResponse (ErrorMessage (T.pack ("Unsupported command: " <> show other))) Nothing
    exitCleanly
----------------------------------------------------------------------------
-- talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------

-- | Receive reverse request responses (such as runInTerminal response)
ack :: Recorder (WithSeverity MainLog)
    -> IORef (Maybe Int)
    -- ^ Reference to PID of runInTerminal proxy process running
    -> ReverseRequestResponse -> DebugAdaptorCont ()
ack l ref rrr = case rrr.reverseRequestCommand of
  ReverseCommandRunInTerminal -> do
    when rrr.success $ do
      logWith l Info $ LaunchLog $ T.pack "RunInTerminal was successful"
  _ -> pure ()

