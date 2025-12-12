{-# LANGUAGE LambdaCase, GADTs #-}
-- | A DSL for evaluating remote expressions on the (possibly) remote debuggee process
--
-- Meant to be imported qualified @as Remote@ for use with @QualifiedDo@.
--
-- @
-- import GHC.Debugger.Runtime.Eval.RemoteExpr (RemoteExpr)
-- import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
-- @
module GHC.Debugger.Runtime.Eval.RemoteExpr
  -- (
  -- ) where
  where

import Prelude hiding (pure, return, (>>=), (>>), fmap, (<$>))
import qualified Prelude
import Data.Typeable
import Data.Bifunctor
import GHC
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp
import Control.Monad.Reader
import GHC.Driver.Config
import GHCi.RemoteTypes

import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Runtime.Eval


--------------------------------------------------------------------------------
-- * Higher-level: eDSL for (possibly) remote evaluation on the debuggee
--------------------------------------------------------------------------------

data RemoteExpr t where
  -- | A top-level or in-another-module name (aka external name) in the debuggee process.
  RemVar    :: Typeable a => ModuleName -> String -> RemoteExpr a

  -- | A reference to a value in the debuggee heap
  RemRef    :: ForeignRef a -> RemoteExpr a

  -- | Apply a remote function to a remote argument to get a remote result
  RemApp    :: RemoteExpr (a -> b) -> RemoteExpr a -> RemoteExpr b

  -- | IO monadic bind on the remote process
  RemBindIO :: RemoteExpr ((a -> IO b) -> IO b) -> (RemoteExpr a -> RemoteExpr (IO b)) -> RemoteExpr (IO b)

-- | Apply a remote function to a remote argument to get a remote result
var :: (Typeable a) => ModuleName -> String -> RemoteExpr a
var = RemVar

-- | A reference to a value in the debuggee heap
ref :: ForeignRef a -> RemoteExpr a
ref = RemRef

-- | Apply a remote function to a remote argument to get a remote result
app :: RemoteExpr (a -> b) -> RemoteExpr a -> RemoteExpr b
app = RemApp

-- | Apply a remote function to a remote 'ForeignRef' to get a remote result
appRef :: RemoteExpr (a -> b) -> ForeignRef a -> RemoteExpr b
appRef rf = RemApp rf . RemRef

-- | IO fmap on the remote process
fmap :: (Typeable a, Typeable b) => (RemoteExpr a -> RemoteExpr b) -> (RemoteExpr (IO a) -> RemoteExpr (IO b))
fmap f io_x = io_x >>= \x -> pure (f x)

(<$>) :: (Typeable a, Typeable b) => (RemoteExpr a -> RemoteExpr b) -> (RemoteExpr (IO a) -> RemoteExpr (IO b))
(<$>) = fmap

-- | IO pure on the remote process
pure :: Typeable a => RemoteExpr a -> RemoteExpr (IO a)
pure = RemApp (var (mkModuleName "GHC.Base") "pure")

-- | IO return on the remote process
return :: Typeable a => RemoteExpr a -> RemoteExpr (IO a)
return = pure

-- | IO monadic bind on the remote process
(>>=) :: forall a b. (Typeable a, Typeable b) => RemoteExpr (IO a) -> (RemoteExpr a -> RemoteExpr (IO b)) -> RemoteExpr (IO b)
(>>=) mx k = RemBindIO (app bind mx) k
  where
    bind :: RemoteExpr (IO a -> (a -> IO b) -> IO b)
    bind = var (mkModuleName "GHC.Base") ">>="

-- | IO monadic bind on the remote process
(>>) :: forall a b. (Typeable a, Typeable b) => RemoteExpr (IO a) -> RemoteExpr (IO b) -> RemoteExpr (IO b)
(>>) ma mb = app (app andThen ma) mb
  where
    andThen :: RemoteExpr (IO a -> IO b -> IO b)
    andThen = var (mkModuleName "GHC.Base") ">>"

--------------------------------------------------------------------------------
-- * Evaluation of 'RemoteExprs' (higher level)
--------------------------------------------------------------------------------

{- |
Evaluate a 'RemoteExpr' for a remote @IO [a]@ and return a list of
'ForeignHValue' with one element per returned @a@.

=== __Example__

@
Remote.evalIOList $ Remote.do
  clonedStack <- remote_cloneThreadStack `Remote.appRef` threadIdRef
  frames      <- remote_decodeStack      `Remote.app`    clonedStack
  return (remote_ssc_stack `Remote.app` frames)
@
-}
evalIOList :: RemoteExpr (IO [a]) -> Debugger (Either BadEvalStatus [ForeignRef a])
evalIOList expr = bimap id (map castForeignRef) Prelude.<$> do
  res_fv <- debuggeeEval expr

  hsc_env <- getSession
  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  handleMultiStatus Prelude.<$> liftIO (
    evalStmt interp eval_opts (EvalThis (castForeignRef res_fv))
    )

-- | Run an @IO a@ computation in the remote process and return the foreign
-- reference to the returned @a@
evalIO :: forall a. Typeable a => RemoteExpr (IO a) -> Debugger (Either BadEvalStatus (ForeignRef a))
evalIO expr = do
  r <- evalIOList (fmap (app singletonList) expr)
  case r of
    Left e    -> Prelude.return (Left e)
    Right [x] -> Prelude.return (Right x)
    _ -> error "impossible: evalIO should only have 1 result"
  where
    singletonList :: RemoteExpr (a -> [a])
    singletonList = var (mkModuleName "Data.List") "singleton"

--------------------------------------------------------------------------------
-- ** Recursive evaluation of 'RemoteExpr' (lower-level)
--------------------------------------------------------------------------------

-- | Get the foreign reference to a heap value of type @a@ in the debuggee process from the given remote expr.
-- This function does not serve to get a remote lambda (@RemoteExpr (a -> b)@).
debuggeeEval :: forall a. RemoteExpr a -> Debugger (ForeignRef a)
debuggeeEval expr = case expr of
  RemVar mod_name var_name -> do
    -- TODO: Lookup mod_var var_name and type in a cache first rather than compile.
    -- Since the @a@ is never parametric in the @Typeable@, we may have many duplicates still (e.g. pure @IO @<varies>).
    -- Measure how many! TODO
    fhv <- compileExprRemote ("(" ++ moduleNameString mod_name ++ "." ++ var_name ++ ") :: " ++ show (typeOf (undefined :: a)))
    Prelude.return (castForeignRef fhv)
  RemRef r -> Prelude.return r
  RemApp f arg -> do
    arg_fv <- debuggeeEval arg
    f_fv   <- debuggeeEval f
    r <- expectRight =<< evalApplication (castForeignRef f_fv) (castForeignRef arg_fv)
    Prelude.return (castForeignRef r)
  RemBindIO iox k -> do
    arg_io_fv <- debuggeeEval iox
    arg_fv    <- expectRight =<< evalThisIO (castForeignRef arg_io_fv)
    debuggeeEval (k (RemRef (castForeignRef arg_fv)))

