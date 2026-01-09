{-# LANGUAGE LambdaCase, DataKinds, TypeFamilies #-}

-- | Higher-level interface to evaluating things in the (possibly remote) debuggee process
module GHC.Debugger.Runtime.Eval
  (
    -- * Raw evaluation
    evalExpr, evalString

    -- ** Error handling
  , handleSingStatus
  , BadEvalStatus(..)

    -- ** Re-exports
  , EvalExpr(..)
  ) where

import GHC
import GHCi.RemoteTypes
import GHCi.Message
import Control.Exception
import GHC.Driver.Env
import GHC.Driver.Config
import Control.Monad.IO.Class
import qualified GHC.Runtime.Interpreter as Interp

import GHC.Debugger.Monad

-- * Evaluating things on debuggee ---------------------------------------------

-- | Evaluate a raw 'EvalExpr' which represents a debuggee expression of type @IO [a]@
evalExpr :: EvalExpr ForeignHValue -> Debugger (Either BadEvalStatus [ForeignHValue])
evalExpr eval_expr = do
  hsc_env <- getSession
  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
  handleMultiStatus <$>
    liftIO (Interp.evalStmt (hscInterp hsc_env) eval_opts eval_expr)

-- | Evaluate a foreign value of type @IO String@ to a @String@
evalString :: ForeignRef (IO String) -> Debugger String
evalString string_fhv = do
  hsc_env <- getSession
  liftIO $
    Interp.evalString (hscInterp hsc_env) (castForeignRef string_fhv)

-- ** Handling evaluation results ----------------------------------------------

-- | Handle the 'EvalStatus_' of an evaluation using 'EvalStepNone' which returns a single value
handleSingStatus :: Either BadEvalStatus [ForeignHValue] -> Either BadEvalStatus ForeignHValue
handleSingStatus status =
  case status of
    Right [sing]  -> Right sing
    Right []      -> Left EvalReturnedNoResults
    Right (_:_:_) -> Left EvalReturnedTooManyResults
    Left e -> Left e

-- | Handle the 'EvalStatus_' of an evaluation using 'EvalStepNone' which returns a list of values
handleMultiStatus :: EvalStatus_ [ForeignHValue] [HValueRef] -> Either BadEvalStatus [ForeignHValue]
handleMultiStatus status =
  case status of
    EvalComplete _ (EvalSuccess res) -> Right res
    EvalComplete _ (EvalException e) ->
      Left (EvalRaisedException (fromSerializableException e))
    EvalBreak {} ->
      --TODO: Could we accidentally hit this if we set a breakpoint regardless of whether EvalStep=None? perhaps.
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
