{-# LANGUAGE LambdaCase, DataKinds, TypeFamilies #-}

-- | Higher-level interface to evaluating things in the (possibly remote) debuggee process
module GHC.Debugger.Runtime.Eval where

import GHC.Conc.Sync
import GHC.Stack.CloneStack
import Control.Monad
import GHC.TypeError
import GHC.TypeLits
import Data.Typeable
import Data.String
import qualified Data.List as L
import GHC
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp
import Control.Monad.Reader
import GHC.Driver.Config
import GHCi.RemoteTypes
import GHCi.Message
import Control.Exception

import GHC.Debugger.Monad
import GHC.Debugger.Utils

-- ** Handling evaluation results ----------------------------------------------

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
