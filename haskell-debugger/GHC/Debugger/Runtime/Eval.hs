-- | Lower-level interface to evaluating things in the (possibly remote) debuggee process
module GHC.Debugger.Runtime.Eval where

import GHC
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp
import Control.Monad.Reader
import GHC.Driver.Config
import GHCi.RemoteTypes
import GHCi.Message
import Control.Exception

import GHC.Debugger.Monad

--------------------------------------------------------------------------------
-- * Evaluation on Foreign Heap Values
--------------------------------------------------------------------------------

-- | Evaluate `f x` for any @f :: a -> b@ and any @x :: a@.
-- The result is the foreign reference to a heap value of type @b@
evalApplication :: ForeignHValue -> ForeignHValue -> Debugger (Either BadEvalStatus ForeignHValue)
evalApplication fref aref = do
  hsc_env <- getSession
  mk_list_fv <- compileExprRemote "(pure @IO . (:[])) :: a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  handleSingStatus <$> liftIO (
    evalStmt interp eval_opts $ (EvalThis mk_list_fv) `EvalApp` ((EvalThis fref) `EvalApp` (EvalThis aref))
    )

-- | Evaluate `f x` for any @f :: a -> IO b@ and any @x :: a@.
-- The result is the foreign reference to a heap value of type @b@ (the IO action is executed)
evalApplicationIO :: ForeignHValue -> ForeignHValue -> Debugger (Either BadEvalStatus ForeignHValue)
evalApplicationIO fref aref = do
  hsc_env <- getSession
  fmap_list_fv <- compileExprRemote "(fmap (:[])) :: IO a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  handleSingStatus <$> liftIO (evalStmt interp eval_opts $ (EvalThis fmap_list_fv) `EvalApp` ((EvalThis fref) `EvalApp` (EvalThis aref)))

-- | Evaluate `f x` for any @f :: a -> IO [b]@ and any @x :: a@.
-- The result is a list of foreign references to the heap values returned in the list of @b@s (the IO action is executed)
evalApplicationIOList :: ForeignHValue -> ForeignHValue -> Debugger (Either BadEvalStatus [ForeignHValue])
evalApplicationIOList fref aref = do
  hsc_env <- getSession

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  handleMultiStatus <$> liftIO (evalStmt interp eval_opts $ (EvalThis fref) `EvalApp` (EvalThis aref))

-- | Handle the 'EvalStatus_' of an evaluation using 'EvalStepNone' which returns a single value
handleSingStatus :: EvalStatus_ [ForeignHValue] [HValueRef] -> Either BadEvalStatus ForeignHValue
handleSingStatus status =
  case status of
    EvalComplete _ (EvalSuccess [res]) -> Right res
    EvalComplete _ (EvalSuccess []) ->
      Left EvalReturnedNoResults
    EvalComplete _ (EvalSuccess (_:_:_)) ->
      Left EvalReturnedTooManyResults
    EvalComplete _ (EvalException e) ->
      Left (EvalRaisedException (fromSerializableException e))
    EvalBreak {} ->
      --TODO: Could we accidentally hit this if we set a breakpoint regardless of whether EvalStep=None? perhaps.
      Left EvalHitUnexpectedBreakpoint

-- | Handle the 'EvalStatus_' of an evaluation using 'EvalStepNone' which returns a list of values
handleMultiStatus :: EvalStatus_ [ForeignHValue] [HValueRef] -> Either BadEvalStatus [ForeignHValue]
handleMultiStatus status =
  case status of
    EvalComplete _ (EvalSuccess res) -> Right res
    EvalComplete _ (EvalException e) ->
      Left (EvalRaisedException (fromSerializableException e))
    EvalBreak {} ->
      Left EvalHitUnexpectedBreakpoint

--------------------------------------------------------------------------------
-- * Exceptions
--------------------------------------------------------------------------------

data BadEvalStatus
  = EvalRaisedException SomeException
  | EvalHitUnexpectedBreakpoint
  | EvalReturnedNoResults
  | EvalReturnedTooManyResults
  deriving Show

instance Exception BadEvalStatus
