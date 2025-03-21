{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass, DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards #-}

-- TODO list:
--
-- [ ]
module Main where

import System.Environment
import Data.Maybe
import Text.Read

import DAP

import Debugger.Interface.Messages hiding (Command, Response)

import Development.Debugger.Init
import Development.Debugger.Breakpoints
import Development.Debugger.Stopped
import Development.Debugger.Evaluation
import Development.Debugger.Interface
import Development.Debugger.Adaptor

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

        -- TODO: Use :print for single-variable, evaluate for the rest. Must match on evaluate req 'hover' type?
        -- Ideally DAP supports "lazy variables" for hover? S.t. hovering doesn't force them unless requested?
      , supportsEvaluateForHovers             = False

      , supportsBreakpointLocationsRequest    = False -- display which breakpoints are valid when user intends to set breakpoint on given line. this happens before actually setting the breakpoint if I understand correctly.
      , supportsConfigurationDoneRequest      = True
      , supportsHitConditionalBreakpoints     = False
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
talk CommandThreads    = commandThreads
talk CommandStackTrace = commandStackTrace
talk CommandScopes     = commandScopes
talk CommandVariables  = commandVariables
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
talk CommandStepOut = do
  -- TODO: How to implement? Perhaps by step-local until the end of the function and then SingleStep?--no, that could still go "inside" the last statement
  -- Do they say in the paper?
  undefined
----------------------------------------------------------------------------
talk CommandEvaluate = commandEvaluate
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
