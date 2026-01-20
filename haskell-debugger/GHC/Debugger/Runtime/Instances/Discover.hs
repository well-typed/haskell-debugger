module GHC.Debugger.Runtime.Instances.Discover
  (
  -- * Runtime 'DebugView' instance
    DebugViewInstance(..)

  -- * Cache for runtime instances
  , RuntimeInstancesCache
  , getDebugViewInstance
  , emptyRuntimeInstancesCache

  -- * Finding runtime instances utils
  , compileAndLoadMthd
  ) where

import Data.IORef
import Data.Function ((&))
import Control.Exception
import Control.Monad.Reader

import GHC
import GHC.Builtin.Names
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Map.Type
import GHC.Driver.Config
import GHC.Driver.Env
import GHC.Driver.Main
import GHC.HsToCore.Expr
import GHC.HsToCore.Monad
import GHC.Plugins
import GHC.Rename.Env
import GHC.Rename.Expr
import GHC.Runtime.Interpreter as Interp
import GHC.Tc.Gen.Expr
import GHC.Tc.Solver
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.Type
import GHCi.Message

import GHC.Debugger.Monad
import GHC.Debugger.Session.Builtin
import Colog.Core as Logger

--------------------------------------------------------------------------------
-- * The Cache-level interface for runtime 'DebugView' instances
--------------------------------------------------------------------------------

-- | Cache 'DebugView' instances found at runtime to avoid trying to find them again.
-- If we found that a particular type doesn't have an instance, we record that as well.
type RuntimeInstancesCache = TypeMap (Maybe DebugViewInstance)

-- | Get a 'DebugViewInstance' for the given type, if one exists.
-- Looks up in the cache and otherwise tries to find the instance.
-- Returns @Nothing@ if no instance could be found.
getDebugViewInstance :: Type -> Debugger (Maybe DebugViewInstance)
getDebugViewInstance ty = do
  rtinMapRef <- asks rtinstancesCache
  rtinMap    <- readIORef rtinMapRef & liftIO
  case lookupTypeMap rtinMap ty of
    Nothing -> do
      res <- findDebugViewInstance ty
      writeIORef rtinMapRef
        (extendTypeMap rtinMap ty res) & liftIO
      return res
    Just res ->
      return res

-- | An empty 'RuntimeInstancesCache'
emptyRuntimeInstancesCache :: RuntimeInstancesCache
emptyRuntimeInstancesCache = emptyTypeMap

--------------------------------------------------------------------------------
-- * Medium level interface for 'DebugView' on 'ForeignHValue's
-- This is cached by GHC.Debugger.Runtime.Instances.Cache
--------------------------------------------------------------------------------

-- | A 'DebugView' instance wrapper to call on values on the (potentially
-- foreign) interpreter heap
data DebugViewInstance = DebugViewInstance
  { -- | 'debugValueIOWrapper' for a specific instance
    instDebugValue  :: ForeignHValue -> IO (Either SomeException ForeignHValue)

    -- | 'debugFieldsIOWrapper' for a specific instance
  , instDebugFields :: ForeignHValue -> IO (Either SomeException ForeignHValue)

    -- | 'VarValueIO' type
    -- todo: pointless to compute this every time... (both of them)
  , varValueIOTy  :: Type
    -- | 'VarFieldsIO' type
  , varFieldsIOTy :: Type
  }

--------------------------------------------------------------------------------
-- * Impl. to find instance and load instance methods applied to right dictionary
--------------------------------------------------------------------------------

-- | Try to find the 'DebugView' instance for a given type using the
-- @haskell-debugger-view@ unit found at session set-up time (see
-- @'hsDbgViewUnitId'@)
findDebugViewInstance :: Type -> Debugger (Maybe DebugViewInstance)
findDebugViewInstance needle_ty = do
  hsc_env <- getSession

  mhdv_uid <- getHsDebuggerViewUid
  case mhdv_uid of
    Just hdv_uid -> do
      let modl = mkModule (RealUnit (Definite hdv_uid)) debuggerViewClassModName
      let mthdRdrName mthStr = mkOrig modl (mkVarOcc mthStr) :: RdrName

      (err_msgs, res) <- liftIO $ runTcInteractive hsc_env $ do

        -- Types used by DebugView
        varValueIOTy    <-  fmap mkTyConTy . tcLookupTyCon
                        =<< lookupTypeOccRn (mkOrig modl (mkTcOcc "VarValueIO"))
        varFieldsIOTy   <-  fmap mkTyConTy . tcLookupTyCon
                        =<< lookupTypeOccRn (mkOrig modl (mkTcOcc "VarFieldsIO"))

        ioTyCon <- tcLookupTyCon ioTyConName

        -- Try to compile and load an expression for all methods of `DebugView`
        -- applied to the dictionary for the given Type (`needle_ty`)
        let debugValueME  = nlHsVar $ mthdRdrName "debugValueIOWrapper"
            debugFieldsME = nlHsVar $ mthdRdrName "debugFieldsIOWrapper"
            debugValueWrapperMT =
              mkVisFunTyMany needle_ty $
                mkTyConApp ioTyCon [mkListTy varValueIOTy]
            debugFieldsWrapperMT =
              mkVisFunTyMany needle_ty $
                mkTyConApp ioTyCon [mkListTy varFieldsIOTy]
        !debugValue_fval  <- compileAndLoadMthd debugValueME  debugValueWrapperMT
        !debugFields_fval <- compileAndLoadMthd debugFieldsME debugFieldsWrapperMT

        let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
            interp    = hscInterp hsc_env

            -- If we hit a breakpoint while evaluating this, just keep going.
            handleStatus (EvalBreak _ _ resume_ctxt _) = do
              resume_ctxt_fhv <- mkFinalizedHValue interp resume_ctxt
              handleStatus =<< Interp.resumeStmt interp eval_opts resume_ctxt_fhv
            -- When completed, return value
            handleStatus (EvalComplete _ (EvalException e)) =
              return (Left (fromSerializableException e))
            handleStatus (EvalComplete _ (EvalSuccess [hval])) =
              return (Right hval)
            handleStatus (EvalComplete _ (EvalSuccess _)) =
              return (Left (SomeException (userError "unexpected more than one value bound for evaluation of DebugView method")))

        return DebugViewInstance
          { instDebugValue = \x_fval -> do
              handleStatus =<< evalStmt interp eval_opts
                (EvalThis debugValue_fval `EvalApp` EvalThis x_fval)
          , instDebugFields = \x_fval -> do
              handleStatus =<< evalStmt interp eval_opts
                (EvalThis debugFields_fval `EvalApp` EvalThis x_fval)
          , varValueIOTy
          , varFieldsIOTy
          }

      case res of
        Nothing -> do
          logSDoc Logger.Debug $
            text "Couldn't compile DebugView instance for" <+> ppr needle_ty $$ ppr err_msgs
          -- The error is for debug purposes. We simply won't use a custom instance:
          return Nothing
        Just is ->
          return $ Just is
    Nothing ->
      -- Custom view is disabled
      return Nothing

-- | Try to compile and load a class method for the given type.
--
-- E.g. @compileAndLoadMthd (nlHsVar "foo") <ty>@ returns the
-- foreign value for an expression @foo@ applied to the dictionary required to
-- produce the final requested type
compileAndLoadMthd :: LHsExpr GhcPs -- ^ Expr of method/expr that takes dictionary
                   -> Type         -- ^ The final type of expr when funct is alredy applied to dict
                   -> TcM ForeignHValue
compileAndLoadMthd expr mthTy = do
  hsc_env <- getTopEnv

  -- Rn, Tc, desugar applied to dictionary
  (expr', _)    <- rnExpr (unLoc expr)
  (expr'', wcs) <- captureConstraints $ tcExpr expr' (Check mthTy)
  ev            <- simplifyTop wcs
  failIfErrsM -- Before Zonking! If solving the constraint failed, `ev == []`.
  let final_exp = mkHsDictLet (EvBinds ev) (noLocA expr'')
  tc_expr_final <- zonkTopLExpr final_exp
  (_, Just ds_expr) <- initDsTc $ dsLExpr tc_expr_final

  -- Compile to a BCO and load it
  (mthd_fval, _, _) <- liftIO $ hscCompileCoreExpr hsc_env noSrcSpan ds_expr

  return mthd_fval
