{-# LANGUAGE LambdaCase, DataKinds, TypeFamilies #-}

-- | Higher-level interface to evaluating things in the (possibly remote) debuggee process
module GHC.Debugger.Runtime.Eval
  ( evalExpr
  , handleSingStatus
  , BadEvalStatus(..)
  ) where

import GHC
import GHC.Runtime.Interpreter as Interp
import GHCi.RemoteTypes
import GHCi.Message
import Control.Exception
import GHC.Driver.Env
import GHC.Driver.Config
import Control.Monad.IO.Class

import GHC.Debugger.Utils
import GHC.Debugger.Monad
import GHC.Debugger.Logger as Logger

-- * Evaluating things on debuggee ---------------------------------------------

-- | Evaluate a raw 'EvalExpr' which represents a debuggee expression of type @IO [a]@
evalExpr :: EvalExpr ForeignHValue -> Debugger (Either BadEvalStatus [ForeignHValue])
evalExpr eval_expr = do
  logSDoc Logger.Debug (text "evalExpr:" <+> (text (show (mkUnit eval_expr))))
  hsc_env <- getSession
  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
  handleMultiStatus <$>
    liftIO (evalStmt (hscInterp hsc_env) eval_opts eval_expr)
  where
    mkUnit :: EvalExpr a -> EvalExpr ()
    mkUnit EvalThis{} = EvalThis ()
    mkUnit (EvalApp a b) = mkUnit a `EvalApp` mkUnit b

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
