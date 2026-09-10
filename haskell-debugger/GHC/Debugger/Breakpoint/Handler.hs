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

-- | Exposes functions that determine how the debugger handles a breakpoint being hit
module GHC.Debugger.Breakpoint.Handler
  ( handleExecResult
  ) where

import GHC.Utils.Outputable
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Function

import GHC qualified
import GHC (
  ExecResult (..),
  execStmt',
  ForeignHValue,
  GhciLStmt,
  GhcPs,
  InteractiveImport (..),
  mkHsString,
  ModSummary (..),
  Name,
  nlHsLit,
  nlList,
  parseImportDecl,
  SingleStep (..),
  SrcSpan (..),
  StmtLR (..),
  unLoc,
  )
import GHC.Plugins (SourceError)
#if MIN_VERSION_ghc(10,1,0)
import GHC.Builtin.Modules (gHC_INTERNAL_GHCI_HELPERS)
#else
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
#endif
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Main (hscParseStmtWithLocation)
import GHC.Driver.Monad as GHC
import GHC.Driver.Env as GHC
import qualified GHC.Driver.Config.Parser as GHC
import GHC.Runtime.Debugger.Breakpoints as GHC
import GHC.Types.Name.Occurrence (mkVarOccFS)
import GHC.Types.Name.Reader as RdrName (mkOrig)
import qualified GHCi.Message as GHCi

import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Interface.Messages
import Colog.Core as Logger
import qualified GHC.Debugger.Breakpoint.Map as BM
import GHC.Debugger.Runtime.Thread.Resume
import GHC.Debugger.Session (setInteractiveDebuggerDynFlags, getInteractiveDebuggerDynFlags, resumeExec)
import Data.List (find)
import GHC.Unit.Module.Graph as GHC
import GHC.Types.Var
import GHC.Runtime.Eval.Types
import GHC.Runtime.Eval (readIModBreaks)
import GHC.ByteCode.Breakpoints

--------------------------------------------------------------------------------

-- | Turn a GHC's 'ExecResult' into an 'EvalResult' response
handleExecResult :: GHC.ExecResult -> Debugger EvalResult
handleExecResult = \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> return (EvalCompleted "" "" Nothing NoVariables) -- Evaluation completed without binding any result.
#if MIN_VERSION_ghc(10,1,0)
        Right (n:_ns) -> inspectId n >>= \case
#else
        Right (n:_ns) -> inspectName n >>= \case
#endif
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
        -- When stepping (`GHC.resumeExec SingleStep` or similar), we will typically stop at locations not explicitly enabled by the user (i.e. not registered in `activeBreakpoints`).
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
    doEval' expr evalFailedMsg br k = doEval expr >>= \case
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

#if MIN_VERSION_ghc(10,1,0)

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectId :: Id -> Debugger (Maybe VarInfo)
inspectId (GHC.AnId -> tt) = Just <$> do
  fam_envs <- getFamInstEnvs'
  tyThingToVarInfo fam_envs tt
#else

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectName :: Name -> Debugger (Maybe VarInfo)
inspectName n = do
  GHC.lookupName n >>= \case
    Nothing -> do
      liftIO . putStrLn =<< display (text "Failed to lookup name: " <+> ppr n)
      pure Nothing
    Just tt -> Just <$> do
      fam_envs <- getFamInstEnvs'
      tyThingToVarInfo fam_envs tt
#endif
