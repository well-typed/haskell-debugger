{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger where

import System.Exit
import Control.Monad.IO.Class

import GHC.Debugger.Breakpoint
import GHC.Debugger.Evaluation
import GHC.Debugger.Stopped
import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Logger

--------------------------------------------------------------------------------
-- * Executing commands
--------------------------------------------------------------------------------

-- | Execute the given debugger command in the current 'Debugger' session
execute :: Recorder (WithSeverity DebuggerLog) -> Command -> Debugger Response
execute recorder = \case
  ClearFunctionBreakpoints -> DidClearBreakpoints <$ clearBreakpoints Nothing
  ClearModBreakpoints fp -> DidClearBreakpoints <$ clearBreakpoints (Just fp)
  SetBreakpoint{brk, hitCount, condition} ->
    DidSetBreakpoint <$> setBreakpoint brk (condBreakEnableStatus hitCount condition)
  DelBreakpoint bp -> DidRemoveBreakpoint <$> setBreakpoint bp BreakpointDisabled
  GetBreakpointsAt ModuleBreak{path, lineNum, columnNum} -> do
    mmodl <- getModuleByPath path
    case mmodl of
      Left e -> do
        displayWarnings [e]
        return $ DidGetBreakpoints Nothing
      Right modl -> do
        mbfnd <- getBreakpointsAt modl lineNum columnNum
        return $
          DidGetBreakpoints (realSrcSpanToSourceSpan . snd <$> mbfnd)
  GetBreakpointsAt _ -> error "unexpected getbreakpoints without ModuleBreak"
  GetStacktrace -> GotStacktrace <$> getStacktrace
  GetScopes -> GotScopes <$> getScopes
  GetVariables kind -> GotVariables <$> getVariables kind
  DoEval exp_s -> DidEval <$> doEval exp_s
  DoContinue -> DidContinue <$> doContinue
  DoSingleStep -> DidStep <$> doSingleStep
  DoStepOut -> DidStep <$> doStepOut
  DoStepLocal -> DidStep <$> doLocalStep
  DebugExecution { entryPoint, entryFile, runArgs } -> DidExec <$> debugExecution (cmapWithSev EvalLog recorder) entryFile entryPoint runArgs
  TerminateProcess -> liftIO $ do
    -- Terminate!
    exitWith ExitSuccess

data DebuggerLog
  = EvalLog EvalLog

instance Pretty DebuggerLog where
  pretty = \ case
    EvalLog msg -> pretty msg
