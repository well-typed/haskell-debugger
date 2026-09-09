{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger where

import GHC.Debugger.Breakpoint
import GHC.Debugger.Run
import GHC.Debugger.Stopped
import GHC.Debugger.Stopped.Exception (getExceptionInfo)
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages

--------------------------------------------------------------------------------
-- * Executing commands
--------------------------------------------------------------------------------

-- | Execute the given debugger command in the current 'Debugger' session
execute :: Command -> Debugger Response
execute = \case
  ClearFunctionBreakpoints -> DidClearBreakpoints <$ clearBreakpoints Nothing
  ClearModBreakpoints fp -> DidClearBreakpoints <$ clearBreakpoints (Just fp)
  SetBreakpoint{brk, hitCount, condition, logMessage} ->
    DidSetBreakpoint <$> setBreakpoint brk (condBreakEnableStatus hitCount condition) (maybe BreakpointStop (BreakpointLogAndResume . logMessageExpression) logMessage)
  DelBreakpoint bp -> DidRemoveBreakpoint <$> setBreakpoint bp BreakpointDisabled BreakpointStop
  GetBreakpointsAt bp -> DidGetBreakpoints <$> getBreakpointsAt bp
  GetThreads -> GotThreads <$> getThreads
  GetStacktrace i -> GotStacktrace <$> getStacktrace i
  GetScopes threadId frameIx -> GotScopes <$> getScopes threadId frameIx
  GetVariables threadId frameIx varRef -> GotVariables <$> getVariables threadId frameIx varRef
  GetExceptionInfo threadId -> GotExceptionInfo <$> getExceptionInfo threadId
  DoEval exp_s -> DidEval <$> doEvalCommand exp_s

-- TODO: We shouldn't block waiting for the result of these operations, because that means we can never resume/step two threads simultaneously. Recall
-- Recall we take things to evaluate from the message queue, but we execute them serially. Here we should do something like `forkIO $ reply`
-- We need an async model that sets them off running and replies once the answer comes back
-- We should just do this in the dap side thread which is running serially

  SetSingleStep
  -- DoContinue mt -> DidContinue <$> doContinue mt
  -- DoSingleStep mt -> DidStep <$> doSingleStep mt
  -- DoStepOut mt -> DidStep <$> doStepOut mt
  -- DoStepLocal mt -> DidStep <$> doLocalStep mt

  DebugExecution { entryPoint, entryFile, runArgs } -> DidExec <$> debugExecution entryFile entryPoint runArgs
