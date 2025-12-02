-- | Lower-level interface to evaluating things in the (possibly remote) debuggee process
module GHC.Debugger.Runtime.Eval where

import GHC
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp
import Control.Monad.Reader
import GHC.Driver.Config
import GHCi.RemoteTypes

import GHC.Debugger.Monad

--------------------------------------------------------------------------------
-- * Evaluation on Foreign Heap Values
--------------------------------------------------------------------------------

-- | Evaluate `f x` for any @f :: a -> b@ and any @x :: a@.
-- The result is the foreign reference to a heap value of type @b@
evalApplication :: ForeignHValue -> ForeignHValue -> Debugger ForeignHValue
evalApplication fref aref = do
  hsc_env <- getSession
  mk_list_fv <- compileExprRemote "(pure @IO . (:[])) :: a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  liftIO (evalStmt interp eval_opts $ (EvalThis mk_list_fv) `EvalApp` ((EvalThis fref) `EvalApp` (EvalThis aref)))
    >>= liftIO . handleSingStatus

-- | Evaluate `f x` for any @f :: a -> IO b@ and any @x :: a@.
-- The result is the foreign reference to a heap value of type @b@ (the IO action is executed)
evalApplicationIO :: ForeignHValue -> ForeignHValue -> Debugger ForeignHValue
evalApplicationIO fref aref = do
  hsc_env <- getSession
  fmap_list_fv <- compileExprRemote "(fmap (:[])) :: IO a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  liftIO (evalStmt interp eval_opts $ (EvalThis fmap_list_fv) `EvalApp` ((EvalThis fref) `EvalApp` (EvalThis aref)))
    >>= liftIO . handleSingStatus

-- | Handle the 'EvalStatus_' of an evaluation using 'EvalStepNone' which returns a single value
handleSingStatus :: MonadFail m => EvalStatus_ [ForeignHValue] [HValueRef] -> m ForeignHValue
handleSingStatus status =
  case status of
    EvalComplete _ (EvalSuccess [res]) -> pure res
    EvalComplete _ (EvalSuccess []) ->
      fail "evaluation did not bind any values"
    EvalComplete _ (EvalSuccess (_:_:_)) ->
      fail "evaluation produced more than one value"
    EvalComplete _ (EvalException e) ->
      fail ("evaluation raised an exception: " ++ show e)
    EvalBreak {} ->
      --TODO: Could we accidentally hit this if we set a breakpoint regardless of whether EvalStep=None? perhaps.
      fail "evaluation unexpectedly hit a breakpoint"

-- | Handle the 'EvalStatus_' of an evaluation using 'EvalStepNone' which returns a list of values
handleMultiStatus :: MonadFail m => EvalStatus_ [ForeignHValue] [HValueRef] -> m [ForeignHValue]
handleMultiStatus status =
  case status of
    EvalComplete _ (EvalSuccess res) -> pure res
    EvalComplete _ (EvalException e) ->
      fail ("evaluation raised an exception: " ++ show e)
      -- TODO?: throwIO (fromSerializableException e)
    EvalBreak {} ->
      fail "evaluation unexpectedly hit a breakpoint"
