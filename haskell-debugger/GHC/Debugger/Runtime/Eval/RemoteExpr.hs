{-# LANGUAGE LambdaCase, GADTs, DataKinds, MagicHash, StandaloneKindSignatures #-}
-- | A DSL for evaluating remote expressions on the (possibly) remote debuggee process
--
-- Meant to be imported qualified @as Remote@ for use with @QualifiedDo@.
--
-- @
-- import GHC.Debugger.Runtime.Eval.RemoteExpr (RemoteExpr)
-- import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
-- @
module GHC.Debugger.Runtime.Eval.RemoteExpr
  (
  -- * Building remote expressions
    RemoteExpr
  , var, ref, untypedRef
  , lit, raw
  , app, appRef
  , withUnboxed
  , fmap, (<$>)
  , pure, return
  , (>>=), (>>)

  -- * Evaluating remote expressions
  , eval, evalIO, evalString, evalIOList, evalIOString
  ) where

import Prelude hiding (pure, return, (>>=), (>>), fmap, (<$>))
import qualified Prelude
import qualified Data.Kind as Kind
import Control.Monad.Except
import GHC.Exts
import GHC
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp hiding (evalIO, evalString)
import qualified GHC.Runtime.Interpreter as Interp
import Control.Monad.Reader
import GHC.Driver.Config
import GHCi.RemoteTypes

import GHC.Debugger.Logger as Logger
import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Runtime.Eval

--------------------------------------------------------------------------------
-- * Higher-level: eDSL for (possibly) remote evaluation on the debuggee
--------------------------------------------------------------------------------

-- | A remote expression to be evaluated on the debuggee process.
-- Note: the remote expression must have a boxed representation type.
type RemoteExpr :: forall l. TYPE (BoxedRep l) -> Kind.Type
data RemoteExpr t where

  -- | Parse, compile, and load a raw expression string onto the remote process.
  -- This is a low-level escape hatch; prefer using the other constructors.
  -- The loaded expression is cached by its string.
  RemRaw    :: String -> RemoteExpr a

  -- | A top-level or in-another-module name (aka external name) in the debuggee process.
  -- The list of strings is the list of (fully-qualified) types the expression
  -- should be applied to.
  RemVar    :: -- forall {l} (a :: TYPE (BoxedRep l))
             ModuleName -> String -> [String] -> RemoteExpr a

  -- | A reference to a value in the debuggee heap
  RemRef    :: forall {l} (a :: TYPE (BoxedRep l))
             . ForeignHValue -> RemoteExpr a

  -- | An Int literal (@Int@)
  RemInt    :: Int -> RemoteExpr Int

  -- | Apply a remote function to a remote argument to get a remote result
  --
  -- Note: the result type @b@ must have a BoxedRep!
  RemApp    :: forall {l} (a :: TYPE (BoxedRep l)) b
             . RemoteExpr (a -> b) -> RemoteExpr a -> RemoteExpr b

  -- | IO monadic bind on the remote process
  RemBindIO :: RemoteExpr (IO a) -> (RemoteExpr a -> RemoteExpr (IO b)) -> RemoteExpr (IO b)

-- | Apply a remote function to a remote argument to get a remote result
var :: -- forall {l} (a :: TYPE (BoxedRep l))
     ModuleName -> String -> [String] -> RemoteExpr a
var = RemVar

-- | A reference to a value in the debuggee heap
ref :: ForeignRef a -> RemoteExpr a
ref = RemRef . castForeignRef

-- | A reference to a value in the debuggee heap
untypedRef :: forall {l} (a :: TYPE (BoxedRep l))
            . ForeignHValue -> RemoteExpr a
untypedRef = RemRef

-- | A literal unboxed int
lit :: Int -> RemoteExpr Int
lit = RemInt

-- | A raw expression string to be parsed, compiled, and loaded onto the remote process
raw :: String -> RemoteExpr a
raw = RemRaw

-- | Apply a remote function to a remote argument to get a remote result
app :: forall {l} (a :: TYPE (BoxedRep l)) b
     . RemoteExpr (a -> b) -> RemoteExpr a -> RemoteExpr b
app = RemApp

-- | Apply a remote function to a remote 'ForeignRef' to get a remote result
appRef :: RemoteExpr (a -> b) -> ForeignRef a -> RemoteExpr b
appRef rf = RemApp rf . ref

-- | Apply a remote function after unboxing a remote int argument
-- (We need to unbox the Int on the debuggee side)
withUnboxed :: RemoteExpr Int -> (RemoteExpr (Int# -> b)) -> RemoteExpr b
withUnboxed i f = (RemRaw "\\f i -> case i of GHC.Exts.I# i# -> f i#")
                    `RemApp` f `RemApp` i

-- | IO fmap on the remote process
fmap :: (RemoteExpr a -> RemoteExpr b) -> (RemoteExpr (IO a) -> RemoteExpr (IO b))
fmap f io_x = io_x >>= \x -> pure (f x)

-- | IO fmap on the remote process
(<$>) :: (RemoteExpr a -> RemoteExpr b) -> (RemoteExpr (IO a) -> RemoteExpr (IO b))
(<$>) = fmap

-- | IO pure on the remote process
pure :: RemoteExpr a -> RemoteExpr (IO a)
pure = RemApp (var (mkModuleName "GHC.Base") "pure" ["IO"])

-- | IO return on the remote process
return :: RemoteExpr a -> RemoteExpr (IO a)
return = pure

-- | IO monadic bind on the remote process
(>>=) :: RemoteExpr (IO a) -> (RemoteExpr a -> RemoteExpr (IO b)) -> RemoteExpr (IO b)
(>>=) = RemBindIO

-- | IO monadic bind on the remote process
(>>) :: RemoteExpr (IO a) -> RemoteExpr (IO b) -> RemoteExpr (IO b)
(>>) ma mb = app (app andThen ma) mb
  where
    andThen :: RemoteExpr (IO a -> IO b -> IO b)
    andThen = var (mkModuleName "GHC.Base") ">>" ["IO"]

--------------------------------------------------------------------------------
-- * Evaluation of 'RemoteExprs' (higher level)
--------------------------------------------------------------------------------

-- | Evaluate a @a@ expression on the remote process and return the foreign
-- reference to the result.
eval :: RemoteExpr a -> Debugger (Either BadEvalStatus (ForeignRef a))
eval expr = evalIO (pure expr)

-- | Run an @IO a@ computation in the remote process and return the foreign
-- reference to the returned @a@
evalIO :: RemoteExpr (IO a) -> Debugger (Either BadEvalStatus (ForeignRef a))
evalIO expr = do
  r <- evalIOList (fmap singletonList expr)
  case r of
    Left e    -> Prelude.return (Left e)
    Right [x] -> Prelude.return (Right x)
    Right []  -> Prelude.return (Left EvalReturnedNoResults)
    Right _   -> Prelude.return (Left EvalReturnedTooManyResults)

-- | Evaluate a string expression on the remote process and return the string
-- to the debugger
evalString :: RemoteExpr String -> Debugger (Either BadEvalStatus String)
evalString expr = evalIOString (pure expr)

{- |
Evaluate a 'RemoteExpr' for a remote @IO [a]@ and return a list of
'ForeignHValue' with one element per returned @a@.

=== __Example__

@
Remote.evalIOList $ Remote.do
  clonedStack <- Remote.cloneThreadStack `Remote.appRef` threadIdRef
  frames      <- Remote.decodeStack      `Remote.app`    clonedStack
  return (Remote.ssc_stack `Remote.app` frames)
@
-}
evalIOList :: RemoteExpr (IO [a]) -> Debugger (Either BadEvalStatus [ForeignRef a])
evalIOList expr = runExceptT $ do
  lift $ logSDoc Logger.Debug (text "evalIOList" <+> text (show expr))

  res_fv <- debuggeeEval expr

  hsc_env <- lift getSession
  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

  r <- handleMultiStatus Prelude.<$> liftIO (
    evalStmt interp eval_opts (EvalThis (castForeignRef res_fv))
    )
  liftEither (map castForeignRef Prelude.<$> r)

-- | Execute an @IO String@ on the remote process and serialize the string back to the debugger.
evalIOString :: RemoteExpr (IO String) -> Debugger (Either BadEvalStatus String)
evalIOString expr = runExceptT $ do
  lift $ logSDoc Logger.Debug (text "evalIOString" <+> text (show expr))
  res_fv <- debuggeeEval expr
  hsc_env <- lift getSession
  liftIO (Interp.evalString (hscInterp hsc_env) (castForeignRef res_fv))

--------------------------------------------------------------------------------
-- ** Recursive evaluation of 'RemoteExpr' (lower-level)
--------------------------------------------------------------------------------

-- | Get the foreign reference to a heap value of type @a@ in the debuggee
-- process from the given remote expr.
--
-- The result can't be (@ForeignRef a@) because of levity polymorphism, so we
-- return the untyped foreign ref.
debuggeeEval :: RemoteExpr a -> ExceptT BadEvalStatus Debugger (ForeignRef a)
debuggeeEval expr = do
    eval_expr <- go (pure (singletonList expr))

    hsc_env <- lift $ getSession
    let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
    r <- lift $ handleSingStatus Prelude.<$>
      liftIO (evalStmt (hscInterp hsc_env) eval_opts eval_expr)
    liftEither (castForeignRef Prelude.<$> r)
  where

    -- Construct the largest possible EvalExpr and then evaluate it all at once.
    -- When we find an IO action we execute it.
    go :: forall {l'} (a' :: TYPE (BoxedRep l'))
        . RemoteExpr a' -> ExceptT BadEvalStatus Debugger (EvalExpr ForeignHValue)
    go = \case
      RemRaw s -> do
        fhv <- lift $ compileExprRemote s
        Prelude.return (EvalThis fhv)
      RemVar mod_name var_name ty_args -> do
        -- TODO: Lookup mod_var var_name and type in a cache first rather than compile.
        fhv <- lift $ compileExprRemote
          ("(" ++ moduleNameString mod_name
               ++ "." ++ var_name ++ ") "
               ++ unwords (map ('@':) ty_args))
        Prelude.return (EvalThis fhv)
      RemRef r -> Prelude.return (EvalThis r)
      RemInt i ->
        -- todo: interpreter message for unboxed literals
        -- TODO: lookup in the cache if we loaded this int already.
        EvalThis Prelude.<$> lift (compileExprRemote (show i))
      RemApp f arg -> do
        arg_e <- go arg
        f_e   <- go f
        Prelude.return (f_e `EvalApp` arg_e)
      RemBindIO iox k -> do

        expr_arg_io_fv <- go (fmapIO singletonListVar iox)

        hsc_env <- lift $ getSession
        let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
        e_arg_fv <- lift $ handleSingStatus Prelude.<$>
          -- EvalStmt takes a type @IO [a]@
          liftIO (evalStmt (hscInterp hsc_env) eval_opts expr_arg_io_fv)
        arg_fv <- liftEither (castForeignRef Prelude.<$> e_arg_fv)
        go (k (ref arg_fv))

instance Show (RemoteExpr (a :: TYPE (BoxedRep l))) where
  show (RemRaw s) = "(" ++ s ++ ")"
  show (RemVar mod_name var_name ty_args) =
    "(" ++ moduleNameString mod_name ++ "." ++ var_name ++ ")"
        ++ (if null ty_args then "" else " ")
        ++ unwords (map ('@':) ty_args)
  show (RemRef _) = "<foreign ref>"
  show (RemInt i) = show i
  show (RemApp f arg) =
    "(" ++ show f ++ " " ++ show arg ++ ")"
  show (RemBindIO iox k) =
    let k_str = k (var (mkModuleName "Dummy") "dummy" [])
     in show iox ++ ">>= (\\dummy -> " ++ show k_str ++ ")"

mkUnit :: EvalExpr a -> EvalExpr ()
mkUnit EvalThis{} = EvalThis ()
mkUnit (EvalApp a b) = mkUnit a `EvalApp` mkUnit b

--------------------------------------------------------------------------------
-- ** Builtins that are needed here too.

-- | Remote 'Data.List.singleton' (applied)
singletonList :: RemoteExpr a -> RemoteExpr [a]
singletonList = app singletonListVar

-- | Remote 'Data.List.singleton'
singletonListVar :: RemoteExpr (a -> [a])
singletonListVar = var (mkModuleName "Data.List") "singleton" []

-- | Remote 'fmap' for IO. Only works with @RemoteExpr (a -> b)@, not
-- @RemoteExpr a -> RemoteExpr b@ (unlike 'fmap').
--
-- We need it to avoid defining the evaluator for @RemBindIO@ in terms of
-- itself.
fmapIO :: RemoteExpr (a -> b) -> RemoteExpr (IO a) -> RemoteExpr (IO b)
fmapIO = app . app (var (mkModuleName "Data.Functor") "fmap" ["IO"])
