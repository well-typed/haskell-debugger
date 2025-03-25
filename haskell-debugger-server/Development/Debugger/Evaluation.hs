{-# LANGUAGE RecordWildCards, OverloadedRecordDot, DuplicateRecordFields #-}
module Development.Debugger.Evaluation where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.IntSet as IS

import DAP

import Debugger.Interface.Messages
import Development.Debugger.Adaptor
import Development.Debugger.Interface

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

--------------------------------------------------------------------------------
-- * Eval
--------------------------------------------------------------------------------

-- | Command for evaluation (includes evaluation-on-hover)
commandEvaluate :: DebugAdaptor ()
commandEvaluate = do
  EvaluateArguments {..} <- getArguments
  DidEval er <- sendSync (DoEval (T.unpack evaluateArgumentsExpression))
  case er of
    EvalStopped{} -> error "impossible, execution is resumed automatically for 'DoEval'"
    EvalAbortedWith e -> do
      -- Evaluation failed, we report it but don't terminate.
      sendEvaluateResponse EvaluateResponse
        { evaluateResponseResult  = T.pack e
        , evaluateResponseType    = T.pack ""
        , evaluateResponsePresentationHint    = Nothing
        , evaluateResponseVariablesReference  = 0
        , evaluateResponseNamedVariables      = Nothing
        , evaluateResponseIndexedVariables    = Nothing
        , evaluateResponseMemoryReference     = Nothing
        }
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

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Handle an EvalResult by sending a stopped or exited event.
--
-- In particular, the result of evaluation is ignored by this function.
-- The 'EvaluateRequest' handler inspects the EvalResult itself and reports on the result.
handleEvalResult :: Bool {-^ Whether we are "stepping" -} -> EvalResult -> DebugAdaptor ()
handleEvalResult stepping er = case er of
  EvalAbortedWith e -> do
    sendOutputErr (T.pack e)
    sendTerminatedEvent defaultTerminatedEvent
    sendExitedEvent (ExitedEvent 43)
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

