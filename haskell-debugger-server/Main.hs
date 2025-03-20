{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass, DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards #-}
module Main where

import Data.Maybe
import System.Environment
import System.FilePath
import Text.Read

import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Text as T

import DAP

import Debugger.Interface.Messages hiding (Command, Response)

import Development.Debugger.Init
import Development.Debugger.Breakpoints
import Development.Debugger.Interface
import Development.Debugger.Adaptor

-- TODO:
--
-- [ ] Set the working directory to the current directory of the Client project (VSCode/...)
main :: IO ()
main = do
  config <- getConfig
  runDAPServer config talk

-- | Fetch config from environment, fallback to sane defaults
getConfig :: IO ServerConfig
getConfig = do
  let
    hostDefault = "0.0.0.0"
    portDefault = 4711
    capabilities = defaultCapabilities
      { exceptionBreakpointFilters            = [ defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "All exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_EXCEPTION
                                                  }
                                                , defaultExceptionBreakpointsFilter
                                                  { exceptionBreakpointsFilterLabel = "Uncaught exceptions"
                                                  , exceptionBreakpointsFilterFilter = BREAK_ON_ERROR
                                                  }
                                                ]
      , supportsBreakpointLocationsRequest    = False -- display which breakpoints are valid when user intends to set breakpoint on given line. this happens before actually setting the breakpoint if I understand correctly.
      , supportsConfigurationDoneRequest      = True
      , supportsHitConditionalBreakpoints     = False
      , supportsEvaluateForHovers             = False -- TODO!!!
      , supportsModulesRequest                = False
      , additionalModuleColumns               = [ defaultColumnDescriptor
                                                  { columnDescriptorAttributeName = "Extra"
                                                  , columnDescriptorLabel = "Label"
                                                  }
                                                ]
      , supportsValueFormattingOptions        = False
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
-- * Executing debuggee
--------------------------------------------------------------------------------

-- | Start executing from entry point
--
-- TODO:
--  [ ] Consider using Output events for debuggee evaluation.
startExecution :: DebugAdaptor EvalResult
startExecution = do
  DAS{entryPoint, entryArgs} <- getDebugSession
  let entry
        | entryPoint == "main" = MainEntry Nothing
        | otherwise            = FunctionEntry entryPoint
  DidExec er <- sendSync DebugExecution{entryPoint = entry, runArgs = entryArgs}
  return er

-- | Handle an EvalResult by sending a stopped or exited event.
--
-- In particular, the result of evaluation is ignored by this function.
-- The 'EvaluateRequest' inspects the EvalResult itself and reports on the result.
handleEvalResult :: Bool {-^ Whether we are "stepping" -} -> EvalResult -> DebugAdaptor ()
handleEvalResult stepping er = case er of
  EvalCompleted{} -> do
    sendTerminatedEvent defaultTerminatedEvent
    sendExitedEvent (ExitedEvent 0)
  EvalException{} -> do
    sendTerminatedEvent defaultTerminatedEvent
    sendExitedEvent (ExitedEvent 42)
  EvalStopped {breakId = Nothing} ->
    sendStoppedEvent
      defaultStoppedEvent {
        stoppedEventAllThreadsStopped = True
      , stoppedEventReason = StoppedEventReasonException
      , stoppedEventHitBreakpointIds = []
      }
  EvalStopped {breakId = Just bid} -> do
    DAS{breakpointMap} <- getDebugSession
    sendStoppedEvent
      defaultStoppedEvent {
        stoppedEventAllThreadsStopped = True
         -- could be more precise here by saying "function breakpoint" rather than always "breakpoint"
      , stoppedEventReason
          = if stepping then StoppedEventReasonStep
                        else StoppedEventReasonBreakpoint
      , stoppedEventHitBreakpointIds
          = maybe [] IS.toList (M.lookup bid breakpointMap)
      }


-- | Command to fetch stack trace
--
-- TODO:
--  [ ] Move to StackTrace module
commandStackTrace :: DebugAdaptor ()
commandStackTrace = do
  StackTraceArguments{..} <- getArguments
  root <- Development.Debugger.Adaptor.projectRoot <$> getDebugSession
  GotStacktrace [f] <- sendSync GetStacktrace
  let
    fullFile = T.pack $
      if isAbsolute f.file then f.file else root </> f.file

    topStackFrame = defaultStackFrame
      { stackFrameId = 0
      , stackFrameName = T.pack f.name
      , stackFrameLine = f.startLine
      , stackFrameColumn = f.startCol
      , stackFrameEndLine = Just f.endLine
      , stackFrameEndColumn = Just f.endCol
      , stackFrameSource = Just defaultSource{sourcePath = Just fullFile}
      }
  sendStackTraceResponse StackTraceResponse
    { stackFrames = [topStackFrame]
    , totalFrames = Just 1
    }

--------------------------------------------------------------------------------
-- * Talk
--------------------------------------------------------------------------------

-- | Main function where requests are received and Events + Responses are returned.
-- The core logic of communicating between the client <-> adaptor <-> debugger
-- is implemented in this function.
talk :: Command -> DebugAdaptor ()
--------------------------------------------------------------------------------
talk CommandInitialize = do
  sendInitializeResponse
--------------------------------------------------------------------------------
talk CommandLaunch = do
  initDebugger =<< getArguments
  sendLaunchResponse   -- ack
  sendInitializedEvent -- our debugger is only ready to be configured after it has launched the session
--------------------------------------------------------------------------------
talk CommandAttach = undefined
--------------------------------------------------------------------------------
talk CommandSetBreakpoints            = commandSetBreakpoints
talk CommandSetFunctionBreakpoints    = commandSetFunctionBreakpoints
talk CommandSetExceptionBreakpoints   = commandSetExceptionBreakpoints
talk CommandSetDataBreakpoints        = undefined
talk CommandSetInstructionBreakpoints = undefined
----------------------------------------------------------------------------
talk CommandLoadedSources = undefined
----------------------------------------------------------------------------
talk CommandConfigurationDone = do
  sendConfigurationDoneResponse
  -- now that it has been configured, start executing until it halts, then send an event
  startExecution >>= handleEvalResult False
----------------------------------------------------------------------------
talk CommandThreads = -- TODO:
  sendThreadsResponse [
    Thread
      { threadId    = 0
      , threadName  = T.pack "dummy thread"
      }
  ]
talk CommandStackTrace = commandStackTrace
talk CommandScopes = -- TODO:
  sendScopesResponse (ScopesResponse [])
talk CommandVariables = -- TODO:
  sendVariablesResponse (VariablesResponse [])
----------------------------------------------------------------------------
talk CommandContinue = do
  DidContinue er <- sendInterleaved DoContinue $
    sendContinueResponse (ContinueResponse True)
  handleEvalResult False er
----------------------------------------------------------------------------
talk CommandNext = do
  DidStep er <- sendInterleaved DoStepLocal sendNextResponse
  handleEvalResult True er
----------------------------------------------------------------------------
talk CommandStepIn = do
  DidStep er <- sendInterleaved DoSingleStep sendStepInResponse
  handleEvalResult True er
----------------------------------------------------------------------------
talk CommandEvaluate = do
  EvaluateArguments {..} <- getArguments
  DidEval er <- sendSync (DoEval (T.unpack evaluateArgumentsExpression))
  case er of
    EvalStopped{} -> error "impossible, execution is resumed automatically"
    _ -> do
      sendEvaluateResponse EvaluateResponse
        { evaluateResponseResult  = T.pack $ resultVal er
        , evaluateResponseType    = T.pack $ resultType er
        , evaluateResponsePresentationHint    = Nothing
        , evaluateResponseVariablesReference  = 0
        , evaluateResponseNamedVariables      = Nothing
        , evaluateResponseIndexedVariables    = Nothing
        , evaluateResponseMemoryReference     = Nothing
        }
----------------------------------------------------------------------------
talk CommandTerminate = do
  destroyDebugSession
  sendTerminatedEvent defaultTerminatedEvent
talk CommandDisconnect = do
  destroyDebugSession
  sendExitedEvent (ExitedEvent 0)
  sendDisconnectResponse
----------------------------------------------------------------------------
talk CommandBreakpointLocations       = undefined
talk CommandModules = sendModulesResponse (ModulesResponse [] Nothing)
talk CommandSource = undefined
talk CommandPause = undefined
talk (CustomCommand "mycustomcommand") = undefined
----------------------------------------------------------------------------
-- talk cmd = logInfo $ BL8.pack ("GOT cmd " <> show cmd)
----------------------------------------------------------------------------
