{-# LANGUAGE LambdaCase, DataKinds, TypeFamilies #-}

-- | Higher-level interface to evaluating things in the (possibly remote) debuggee process
module GHC.Debugger.Runtime.Eval where

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

--------------------------------------------------------------------------------
-- * Higher-level: eDSL for (possibly) remote evaluation on the debuggee
--------------------------------------------------------------------------------

data RemoteExpr t where
  -- | A top-level or in-another-module name (aka external name) in the debuggee process.
  RemVar   :: Typeable a => ModuleName -> String -> RemoteExpr a

  -- | A reference to a value in the debuggee heap
  RemRef   :: ForeignRef a -> RemoteExpr a

  -- | Apply a remote function to a remote argument to get a remote result
  RemApp   :: (Typeable a) => RemoteExpr (a -> b) -> RemoteExpr a -> RemoteExpr b

  -- | IO monadic bind on the remote process
  RemBind  :: RemoteExpr (IO a) -> (RemoteExpr a -> RemoteExpr (IO b)) -> RemoteExpr (IO b)

  -- | IO pure on the remote process
  RemPure  :: RemoteExpr a      -> RemoteExpr (IO a)

x <- (something :: RemoteExpr a)
f x , f :: (a{-remote-} -> RemoteExpr{-guards that the `a` doesn't escape to debugger-} a)


(>>=) :: RemoteExpr (IO a) -> (RemoteExpr a -> RemoteExpr (IO b)) -> RemoteExpr (IO b)

remote_cloneThreadStack :: ForeignRef ThreadId -> RemoteExprM (IO StackSnapshot)
remote_cloneThreadStack r =
  RemVar (mkModuleName "GHC.Stack.CloneStack") "cloneThreadStack" `RemApp` RemRef r

{-
Evaluate a 'RemoteExprM' of a remote @IO [a]@ and return a list of
'ForeignHValue' with one element per returned @a@.

=== __Example__

@
evalIOList $ RemoteExpr.do
  clonedStack :: RemoteExpr a <- remote_cloneThreadStack threadIdRef :: RemoteExpr (IO a)
  frames      <- remote_decodeStack      clonedStack
  return (remote_ssc_stack frames)
@
-}
evalIOList :: RemoteExprM (IO [a]) -> Debugger [ForeignHValue]
evalIOList


--------------------------------------------------------------------------------

instance Typeable a => Show (RemoteExpr a) where
  show (RemVar mod_name var_name) = moduleNameString mod_name ++ "." ++ var_name ++ " :: " ++ show (typeOf (undefined :: a))
  show (RemRef _)                 = "RemRef <foreign ref>"
  show (RemApp a b)               = "RemApp (" ++ show a ++ ") (" ++ show b ++ ")"

-- | Get the foreign reference to a heap value of type @a@ in the debuggee process from the given remote expr.
-- This function does not serve to get a remote lambda (@RemoteExpr (a -> b)@).
debuggeeHValue :: forall a. Typeable a => RemoteExpr a -> Debugger (ForeignRef a)
debuggeeHValue expr = case expr of
  RemVar mod_name var_name -> do
    -- TODO: Lookup mod_var var_name and type in a cache first rather than compile.
    fhv <- compileExprRemote (moduleNameString mod_name ++ "." ++ var_name ++ " :: " ++ show (typeOf (undefined :: a)))
    return (castForeignRef fhv)
  RemRef ref -> return ref
  RemApp f arg -> do
    arg_fv <- debuggeeHValue arg
    f_fv   <- debuggeeHValue arg
    r <- expectRight =<< evalApplication (castForeignRef f_fv) (castForeignRef arg_fv)
    return (castForeignRef r)

debuggeeEvalIOList = undefined

--------------------------------------------------------------------------------
-- * Lower-level: Evaluation on Foreign Heap Values
--------------------------------------------------------------------------------

-- | Evaluate `f x` for any @f :: a -> b@ and any @x :: a@.
-- The result is the foreign reference to a heap value of type @b@
evalApplication :: ForeignHValue -> ForeignHValue -> Debugger (Either BadEvalStatus ForeignHValue)
evalApplication fref aref = evalApplicationExpr ((EvalThis fref) `EvalApp` (EvalThis aref))

-- | Evaluate `f x y` for any @f :: a -> b ->@ and any @x :: a, y :: b@.
-- The result is the foreign reference to a heap value of type @c@
evalApplication2 :: ForeignHValue -> ForeignHValue -> ForeignHValue -> Debugger (Either BadEvalStatus ForeignHValue)
evalApplication2 fref aref bref = evalApplicationExpr ((EvalThis fref) `EvalApp` (EvalThis aref) `EvalApp` (EvalThis bref))

-- | Evaluate the given 'EvalExpr' of type @b@ in the debuggee process.
-- The result is the foreign reference to a heap value of type @b@
evalApplicationExpr :: EvalExpr ForeignHValue -> Debugger (Either BadEvalStatus ForeignHValue)
evalApplicationExpr eval_expr = do
  hsc_env <- getSession
  mk_list_fv <- compileExprRemote "(pure @IO . (:[])) :: a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  handleSingStatus <$> liftIO (
    evalStmt interp eval_opts $ (EvalThis mk_list_fv) `EvalApp` eval_expr
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

-- | Evaluate `x` of type `String` on the debuggee process and return it on the debugger.
evalStringValue :: ForeignHValue -> Debugger (Either BadEvalStatus String)
evalStringValue string_fv = do
  pure_fv      <- compileExprRemote "(pure @IO) :: String -> IO String"
  evalApplication pure_fv string_fv >>= \case
    Left err -> return (Left err)
    Right string_io_fv -> Right <$> do
      hsc_env <- getSession
      liftIO $ evalString (hscInterp hsc_env) string_io_fv

-- | Evaluate `x` for any @x :: a@.
-- The result is the foreign reference to a heap value of type @a@
evalThis :: ForeignHValue -> Debugger (Either BadEvalStatus ForeignHValue)
evalThis aref = do
  hsc_env <- getSession
  mk_list_fv <- compileExprRemote "(pure @IO . (:[])) :: a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  handleSingStatus <$> liftIO (
    evalStmt interp eval_opts $ (EvalThis mk_list_fv) `EvalApp` (EvalThis aref)
    )

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
