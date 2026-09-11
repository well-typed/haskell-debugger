{-# LANGUAGE RecordWildCards #-}

-- | Evaluating string-expressions in the context of the source program.
-- This is what is used to evaluate expressions at the REPL while stopped.
--
-- In contrast, @GHC.Debugger.Runtime.Eval@ is for evaluating any and all
-- expressions in the (external) interpreter for the debugger purposes (rather
-- than directly the user's).
module GHC.Debugger.Run.Eval
  ( doEval
  ) where
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Monad
import GHC.Debugger.Session
import GHC.Debugger.Runtime.Thread.Resume
import GHC.Debugger.Run.Resume
import Control.Exception
import GHC.Runtime.Eval hiding (resumeExec)
import GHC.Hs
import GHC.Driver.Session
import GHC.Runtime.Context
import qualified GHC
import GHC.Unit.Module.ModSummary
import GHC.ByteCode.Breakpoints
import GHC.Driver.Monad
import Control.Monad
import GHC.Driver.Env
import GHC.Driver.Main
import GHC.Types.SrcLoc
import Control.Monad.Catch as MC
import Data.Function

-- | Evaluate expression including the interactive context at the breakpoint
-- the given thread is currently stopped at. If the thread is not stopped, the
-- expression is evaluated without additional context.
doEval :: (ExecResult -> Debugger EvalResult)
       -- ^ How to handle a result from evaluation.
       -- Typically this is 'GHC.Debugger.Run.Handler.handleExecResult'.
       -> Maybe (RemoteThreadId, Int)
       -- ^ If Just, evaluate the expression in a context where the variables
       -- from the given thread at frame index are available.
       -> String
       -> Debugger EvalResult
doEval handleExecResult mtid expr = withThreadBreakEnv mtid $ do
  excr <- (Right <$> exec expr execOptions) `MC.catch` \(e::SomeException) -> pure (Left (displayException e))
  case excr of
    Left err -> pure $ EvalAbortedWith err
    Right (k, br@ExecBreak{}) -> fmap (addSourceKind k) $ execBreakResume br >>= continueToCompletion >>= handleExecResult
    Right (k, r@ExecComplete{}) -> fmap (addSourceKind k) $ handleExecResult r
  where
    exec :: String -> ExecOptions -> Debugger (SourceKind, ExecResult)
    exec input exec_opts@ExecOptions{..} = do
      hsc_env <- getSession

      mb_stmt <-
        liftIO $
        runInteractiveHsc hsc_env $
        hscParseStmtWithLocation execSourceFile execLineNumber input

      case mb_stmt of
        -- empty statement / comment
        Nothing -> return (IsStmt, ExecComplete (Right []) 0)
        Just stmt -> (,) <$> stmtKind stmt <*> execStmt' stmt input exec_opts

    stmtKind (stmt :: GhciLStmt GhcPs) = do
      pure $ case unLoc stmt of
        BodyStmt{} -> IsExpr
        _ -> IsStmt

    addSourceKind :: SourceKind -> EvalResult -> EvalResult
    addSourceKind k EvalCompleted{..} = EvalCompleted{resultSourceKind = Just k, ..}
    addSourceKind _ r = r

-- | Resume execution with single step mode 'RunToCompletion', skipping all breakpoints we hit, until we reach 'ExecComplete'.
--
-- We use this in 'doEval' because we want to ignore breakpoints in expressions given at the prompt.
continueToCompletion :: Resume -> Debugger ExecResult
continueToCompletion r = do
  execr <- resumeExec RunToCompletion Nothing r
  case execr of
    br@ExecBreak{} -> execBreakResume br >>= continueToCompletion
    ExecComplete{} -> return execr

-- | @withThreadBreakEnv mtid m@ executes @m@ with the imports, language, and language
--  extensions of the <tid>'s breakpoint source module.
--
--  If <tid> is Nothing or not stopped at a breakpoint @m@ is executed with no change.
withThreadBreakEnv :: Maybe (RemoteThreadId, Int) -> Debugger a -> Debugger a
withThreadBreakEnv Nothing                         m = m
withThreadBreakEnv (Just (tid, _TODO_frame_index)) m = do
  hug  <- hsc_HUG <$> getSession
  mibi <- join . fmap resumeBreakpointId <$> readResume tid
  case mibi of
    Nothing -> m
    Just ibi -> do
      breakModule <- getBreakSourceMod ibi <$> readIModBreaks hug ibi & liftIO

      ic_dyn_flags <- getInteractiveDebuggerDynFlags
      break_dyn_flags <- ms_hspp_opts <$> GHC.getModSummary breakModule
      old_context <- getContext
      setInteractiveDebuggerDynFlags $ adjustFlags ic_dyn_flags break_dyn_flags
      setContext (IIModule breakModule : old_context)
      x <- m
      GHC.setContext old_context
      setInteractiveDebuggerDynFlags ic_dyn_flags
      return x
  where
    -- Possibly we might want to include more from the module's DynFlags.
    -- However some are likely to mess with the REPL, e.g. Opt_WarnTypeDefaults,
    -- Opt_HideAllPackages, Opt_NoIt. See discussion at
    -- https://github.com/well-typed/haskell-debugger/pull/230#discussion_r2986758826
    adjustFlags :: DynFlags -> DynFlags -> DynFlags
    adjustFlags ic modl = ic
      { extensions = extensions modl
      , extensionFlags = extensionFlags modl
      , language = language modl
      }
