{-# LANGUAGE RecordWildCards, OverloadedRecordDot, DuplicateRecordFields #-}
module Development.Debug.Adapter.Evaluation where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.IntSet as IS

import DAP

import GHC.Debugger.Interface.Messages
import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import qualified Development.Debug.Adapter.Output as Output

--------------------------------------------------------------------------------
-- * Executing debuggee
--------------------------------------------------------------------------------

-- | Start executing from entry point
--
-- TODO:
--  [ ] Consider using Output events for debuggee evaluation.
startExecution :: DebugAdaptor EvalResult
startExecution = do
  DAS{entryFile, entryPoint, entryArgs} <- getDebugSession
  let entry
        | entryPoint == "main" = MainEntry Nothing
        | otherwise            = FunctionEntry entryPoint
  DidExec er <- sendSync DebugExecution{entryPoint = entry, entryFile, runArgs = entryArgs}
  return er

--------------------------------------------------------------------------------
-- * Eval
--------------------------------------------------------------------------------

-- | Command for evaluation (includes evaluation-on-hover)
commandEvaluate :: DebugAdaptor ()
commandEvaluate = do
  EvaluateArguments {evaluateArgumentsFrameId=_todo, ..} <- getArguments

  let simpleEvalResp res ty = EvaluateResponse
        { evaluateResponseResult             = res
        , evaluateResponseType               = ty
        , evaluateResponsePresentationHint   = Nothing
        , evaluateResponseVariablesReference = 0
        , evaluateResponseNamedVariables     = Nothing
        , evaluateResponseIndexedVariables   = Nothing
        , evaluateResponseMemoryReference    = Nothing
        }

  DidEval er <- sendSync (DoEval (T.unpack evaluateArgumentsExpression))
  case er of
    EvalStopped{} -> error "impossible, execution is resumed automatically for 'DoEval'"
    EvalAbortedWith e ->
      -- Evaluation failed, we report it but don't terminate.
      sendEvaluateResponse (simpleEvalResp (T.pack e) (T.pack ""))
    EvalException {resultVal, resultType} ->
      sendEvaluateResponse (simpleEvalResp (T.pack resultVal) (T.pack resultType))
    EvalCompleted{resultVal, resultType, resultStructureRef} -> do
      sendEvaluateResponse EvaluateResponse
        { evaluateResponseResult             = T.pack resultVal
        , evaluateResponseType               = T.pack resultType
        , evaluateResponsePresentationHint   = Nothing
        , evaluateResponseVariablesReference = fromEnum resultStructureRef
        , evaluateResponseNamedVariables     = Nothing
        , evaluateResponseIndexedVariables   = Nothing
        , evaluateResponseMemoryReference    = Nothing
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
    Output.console (T.pack e)
    sendTerminatedEvent defaultTerminatedEvent
    sendExitedEvent (ExitedEvent 43)
  EvalCompleted{resultVal, resultType} -> do
    Output.console (T.pack $ "Evaluation returned: " ++ resultVal ++ " :: " ++ resultType)
    sendTerminatedEvent defaultTerminatedEvent
    sendExitedEvent (ExitedEvent 0)
  EvalException{resultVal, resultType} -> do
    Output.stderr (T.pack $ "Uncaught exception of type " ++ resultType ++ " was thrown!")
    Output.stderr (T.pack resultVal)
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

