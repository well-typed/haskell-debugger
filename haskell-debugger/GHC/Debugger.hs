{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}


-- | Entry point to execute debugging commands
module GHC.Debugger
  (
    -- * Execute debugger commands
    execute

    -- * Re-export command types
  , module GHC.Debugger.Interface.Messages

  ) where

import qualified GHC.Debugger.Breakpoint as Break
import qualified GHC.Debugger.Run        as Run
import qualified GHC.Debugger.Stopped    as Stopped
import GHC.Debugger.Stopped.Exception (getExceptionInfo)
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages

--------------------------------------------------------------------------------
-- * Executing commands
--------------------------------------------------------------------------------

-- | Execute the given debugger command in the current 'Debugger' session
execute :: Command -> Debugger Response
execute = \case

  ------------------------------------------------------------------------------
  -- GHC.Debugger.Breakpoint
  ------------------------------------------------------------------------------

  ClearFunctionBreakpoints -> DidClearBreakpoints <$
    Break.clearBreakpoints Nothing
  ClearModBreakpoints fp -> DidClearBreakpoints <$
    Break.clearBreakpoints (Just fp)
  SetBreakpoint{brk, hitCount, condition, logMessage} -> DidSetBreakpoint <$>
    Break.setBreakpoint brk
                  (Break.condBreakEnableStatus hitCount condition)
                  (maybe BreakpointStop (BreakpointLogAndResume . Break.logMessageExpression) logMessage)
  DelBreakpoint bp -> DidRemoveBreakpoint <$>
    Break.setBreakpoint bp BreakpointDisabled BreakpointStop
  GetBreakpointsAt bp -> DidGetBreakpoints <$>
    Break.getBreakpointsAt bp

  ------------------------------------------------------------------------------
  -- GHC.Debugger.Stopped
  ------------------------------------------------------------------------------

  GetThreads -> GotThreads <$>
    Stopped.getThreads -- should this really be in Stopped?
  GetStacktrace i -> GotStacktrace <$>
    Stopped.getStacktrace i
  GetScopes threadId frameIx -> GotScopes <$>
    Stopped.getScopes threadId frameIx
  GetVariables threadId frameIx varRef -> GotVariables <$>
    Stopped.getVariables threadId frameIx varRef
  GetExceptionInfo threadId -> GotExceptionInfo <$>
    getExceptionInfo threadId

  ------------------------------------------------------------------------------
  -- GHC.Debugger.Run
  ------------------------------------------------------------------------------

  DoEval exp_s -> DidEval <$>
    Run.doEvalCommand exp_s

  -- TODO: We shouldn't block waiting for the result of these operations, because that means we can never resume/step two threads simultaneously. Recall
  -- Recall we take things to evaluate from the message queue, but we execute them serially. Here we should do something like `forkIO $ reply`
  -- We need an async model that sets them off running and replies once the answer comes back
  -- We should just do this in the dap side thread which is running serially
  --
  -- Even more generally, I think when we read an handleExecResult we should
  -- probably clear the MVar as soon as possible so we can receive as many paused
  -- thread hits as possible and emit thread paused events.
  DoResume tid step world -> DidResume <$>
    Run.doResume tid step world

  DebugExecution { entryPoint, entryFile, runArgs } -> DidExec <$>
    Run.debugExecution entryFile entryPoint runArgs
