{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger where

import Control.Monad.IO.Class

import qualified GHC (getSession)
import GHC.Driver.Env (hscInterp)
import GHC.Runtime.Interpreter (stopInterp)
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
  DoContinue -> DidContinue <$> doContinue
  DoSingleStep -> DidStep <$> doSingleStep
  DoStepOut -> DidStep <$> doStepOut
  DoStepLocal -> DidStep <$> doLocalStep
  DebugExecution { entryPoint, entryFile, runArgs } -> DidExec <$> debugExecution entryFile entryPoint runArgs
  TerminateDebuggee -> DidTerminateDebuggee <$ terminateDebuggee

-- | Stop the debuggee gracefully
-- (idempotent, stopInterp for InterpPending will do nothing)
terminateDebuggee :: Debugger ()
terminateDebuggee = do
  interp <- hscInterp <$> GHC.getSession
  liftIO $ stopInterp interp
