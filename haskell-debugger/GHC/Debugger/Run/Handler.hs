{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Determine how the debugger handles a breakpoint being hit
module GHC.Debugger.Run.Handler
  ( handleExecResult
  ) where

import GHC.Utils.Outputable
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef

import GHC (
  ExecResult (..),
  SingleStep (..),
  )
#if MIN_VERSION_ghc(10,1,0)
import GHC.Builtin.Modules (gHC_INTERNAL_GHCI_HELPERS)
#endif

import Colog.Core as Logger
import GHC.Runtime.Eval.Types

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Monad
import GHC.Debugger.Run.Resume
import GHC.Debugger.Runtime.Thread.Resume

import qualified GHC.Debugger.Data.BreakpointMap as BM
import GHC.Debugger.Run.Eval
import GHC.Debugger.Stopped.Variables

--------------------------------------------------------------------------------

-- | Turn a GHC's 'ExecResult' into an 'EvalResult' response
handleExecResult :: GHC.ExecResult -> Debugger EvalResult
handleExecResult = \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> return (EvalCompleted "" "" Nothing NoVariables) -- Evaluation completed without binding any result.
        Right (n:_ns) -> idToVarInfo n >>= \case
          Just VarInfo{varValue, varType, varRef} -> do
            return (EvalCompleted varValue varType Nothing varRef)
          Nothing     -> liftIO $ fail "doEval failed"
    br@ExecBreak {breakPointId = Nothing} -> do
      -- Stopped at an exception
      -- TODO: force the exception to display string with Backtrace?
      res   <- execBreakResume br
      pushResume res
      rt_id <- getResumeThreadId res
      return EvalStopped{ breakId = Nothing
                        , breakThread = rt_id }
    br@ExecBreak {breakPointId = Just bid} -> do

      res <- execBreakResume br
      pushResume res

      let performAction BreakpointStop = do
                rt_id <- getResumeThreadId res
                return EvalStopped{ breakId = Just bid
                                  , breakThread = rt_id }
          performAction (BreakpointLogAndResume logExpr) = do
            let evalFailedMsg e = text $ unlines ["Evaluation of log message expression failed with " ++ e
                  , "Expr: " ++ logExpr
                  , "Ignoring..."]
            doEval' logExpr evalFailedMsg res $ \ _ _ -> resume res

      bm <- liftIO . readIORef =<< asks activeBreakpoints
      case BM.lookup bid bm of
        -- When stepping (`GHC.resumeExec SingleStep` or similar), we will
        -- typically stop at locations not explicitly enabled by the user (i.e.
        -- not registered in `activeBreakpoints`).
        Nothing -> performAction BreakpointStop
        Just BreakpointInfo{bpInfoStatus = status, bpInfoAction = action} -> do
          case status of
           -- todo: BreakpointAfterCountCond is not handled yet.
            BreakpointAfterCountCond{} -> performAction action
            BreakpointWhenCond cond -> do
              let evalFailedMsg e = text $ "Evaluation of conditional breakpoint expression failed with " ++ e ++ "\nIgnoring..."

              doEval' cond evalFailedMsg res $ \ resultVal resultType -> do
                if resultType == "Bool" then do
                  if resultVal == "True" then do
                    performAction action
                  else
                    resume res
                else do
                  logSDoc Logger.Warning (evalFailedMsg "\"expression resultType is != Bool\"")
                  resume res
            BreakpointDisabled -> resume res
            -- The counting is handled by @GHC.setupBreakpoint@
            BreakpointAfterCount _ -> performAction action
            BreakpointEnabled -> performAction action
  where
    doEval' expr evalFailedMsg br k = do
      rt_id <- getResumeThreadId br
      doEval handleExecResult (Just (rt_id, 0)) expr >>= \case
        EvalStopped{} -> error "impossible for doEval"
        EvalCompleted { resultVal, resultType } ->
          k resultVal resultType
        EvalException { resultVal } -> do
          logSDoc Logger.Warning (evalFailedMsg resultVal)
          resume br
        EvalAbortedWith e -> do
          logSDoc Logger.Warning (evalFailedMsg e)
          resume br
    resume r = resumeExec GHC.RunToCompletion Nothing r >>= handleExecResult
