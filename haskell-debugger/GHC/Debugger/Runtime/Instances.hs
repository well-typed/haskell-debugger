{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments #-}
module GHC.Debugger.Runtime.Instances where

import Control.Exception
import Control.Monad
import Control.Monad.Reader

import GHC
import GHC.Builtin.Names
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Driver.Config
import GHC.Driver.Env
import GHC.Driver.Main
import GHC.HsToCore.Expr
import GHC.HsToCore.Monad
import GHC.Plugins
import GHC.Rename.Env
import GHC.Rename.Expr
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import GHC.Tc.Gen.Expr
import GHC.Tc.Solver
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.Type
import GHC.Types.Error
import GHC.Utils.Logger
import GHCi.Message

import GHC.Debugger.Monad
import GHC.Debugger.View.Class

--------------------------------------------------------------------------------
-- * High level interface for 'DebugView' on 'Term's
--------------------------------------------------------------------------------

-- | Get the custom representation of this 'Term' by applying a 'DebugView'
-- instance 'debugValue' method if there is one.
debugValueTerm :: Term -> Debugger (Maybe VarValue)
debugValueTerm term = do
  hsc_env <- getSession
  let interp = hscInterp hsc_env
  let ty = termType term
  mbInst <- findDebugViewInstance ty
  case mbInst of
    Nothing -> return Nothing
    Just DebugViewInstance
      {instDebugValue, varValueIOTy} -> do
        liftIO (instDebugValue (val term)) >>= \case
          Left _e ->
            -- exception! ignore.
            return Nothing
          Right transformed_v -> do

            liftIO (cvObtainTerm hsc_env maxBound True varValueIOTy transformed_v) >>= \case

              -- Get the Term of the VarValue to decode fields
              Term{ ty=_{-assert==VarValueIO-}
                  , subTerms=[strTerm, boolTerm]
                  } -> do

                valStr <- liftIO $
                  evalString interp (val strTerm {- whose type is IO String, from varValueIO -})

                let valBool = case boolTerm of
                      Term{dc=Left "False"} -> False
                      Term{dc=Left "True"}  -> True
                      Term{dc=Right dc}
                        | falseDataCon == dc -> False
                      Term{dc=Right dc}
                        | trueDataCon == dc -> True
                      _ -> error "Decoding of VarValue failed"

                return $ Just VarValue
                  { varValue = valStr
                  , varExpandable = valBool
                  }
              _ ->
                -- Unexpected; the Term of VarValue should always be Term.
                return Nothing

-- | Get the custom representation of this 'Term' by applying a 'DebugView'
-- instance 'debugFields' method if there is one.
--
-- Returns the mappings from field labels to terms, where each term records the
-- type and pointer to the foreign heap value returned in the instance for that label.
--
-- Returns @Nothing@ if no instance was found for the type of the given term
debugFieldsTerm :: Term -> Debugger (Maybe [(String, Term)])
debugFieldsTerm term = do
  hsc_env <- getSession
  let interp = hscInterp hsc_env
  let ty = termType term
  mbInst <- findDebugViewInstance ty
  case mbInst of
    Nothing -> return Nothing
    Just DebugViewInstance
      {instDebugFields, varFieldsIOTy} -> do
        liftIO (instDebugFields (val term)) >>= \case
          Left _e ->
            -- exception! ignore.
            return Nothing
          Right transformed_v -> do

            liftIO (cvObtainTerm hsc_env 2 True varFieldsIOTy transformed_v) >>= \case

              -- Get the Term of the VarFieldsIO
              NewtypeWrap
                { wrapped_term=fieldsListTerm
                } -> do

                fieldsTerms <- listTermToTermsList fieldsListTerm

                -- Process each term for the instance fields
                Just <$> forM fieldsTerms \fieldTerm0 -> liftIO $ do
                  -- Expand @(IO String, VarFieldValue)@ tuple term for each field
                  seqTerm hsc_env fieldTerm0 >>= \case
                    Term{subTerms=[ioStrTerm, varFieldValTerm]} -> do

                      fieldStr <- evalString interp (val ioStrTerm)

                      -- Expand VarFieldValue term
                      seqTerm hsc_env varFieldValTerm >>= \case
                        Term{subTerms=[unexpandedValueTerm]} -> do
                          actualValueTerm <- liftIO $ do
                            let val_ty = termType unexpandedValueTerm
                            cvObtainTerm hsc_env defaultDepth False{-don't force-} val_ty (val unexpandedValueTerm)
                          return (fieldStr, actualValueTerm)

                        _ -> error "impossible; expected VarFieldValue"
                    _ -> error "impossible; expected 2-tuple term"
              _ -> error "debugFields instance returned something other than VarFields"

-- | Convert a Term representing a list @[a]@ to a list of the terms of type
-- @a@, where @a@ is the given @'Type'@ arg.
--
-- PRE-CON: Term represents a @[a]@
listTermToTermsList :: Term -> Debugger [Term]
listTermToTermsList Term{subTerms=[head_term, tail_term]}
  = do
    hsc_env <- getSession
    -- Expand next term:
    tail_term' <- liftIO $
      seqTerm hsc_env tail_term
    (head_term:) <$> listTermToTermsList tail_term'
listTermToTermsList _ = pure []

--------------------------------------------------------------------------------
-- * Medium level interface for 'DebugView' on 'ForeignHValue's
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

-- As long as the user depends on GHC.Debugger.View.Class somewhere in their full transitive closure,
--  then we get haskell-debugger-view unit-id from the module graph; and all
--  built-in instances are available because they're defined in that module.
--
-- If it's NOT anywhere in the closure, we want to load it ourselves to bring
--  the built-in instances into scope.

{-
How to :
1. Make a unit id for this in memory package
2. Make a ModSummary for each of the modules in haskell-debugger-view
  2.1. Probably summariseFile with the StringBuffer argument

--
3. Call 'compileOne' function on the ModSummary to know whether it will work or not
4. Get HomeModInfo then add it to the HUG/HPT ?
--
Alternatively:
If I knew it was going to compile, I could just load it into the interactive
context directly?
--
Main issue: how to setup the environment for the home package?
When I create the home package I have to pass some package flags
If I want to use e.g. containers for some modules I need to find the right
unit-id of containers that the user is using to pick the right one.

I could just get the module graph from the user program and just use all of them since that's the "maximal" set

If containers is not in the existing build plan then no need to try and compile that module
(If load to int. context did work)

--------------------------------------------------------------------------------
Perhaps more easily:

Just get the user module graph and inject the modules

Add to the module graph a ModSummary node for all of the haskell-debugger-view
modules and try to load the module graph whole again.
Use    | LoadDependenciesOf HomeUnitModule for 'load'
-}

findDebugViewInstance :: Type -> Debugger (Maybe DebugViewInstance)
findDebugViewInstance needle_ty = do
  hsc_env <- getSession
  logger  <- getLogger

  mhdv_uid <- getHsDebuggerViewUid
  case mhdv_uid of
    Just hdv_uid -> liftIO $ do
      let modl = mkModule (RealUnit (Definite hdv_uid)) (mkModuleName "GHC.Debugger.View.Class")
      let mthdRdrName mthStr = mkOrig modl (mkVarOcc mthStr)

      (err_msgs, res) <- runTcInteractive hsc_env $ do

        -- Types used by DebugView
        varValueIOTy    <-  fmap mkTyConTy . tcLookupTyCon
                        =<< lookupTypeOccRn (mkOrig modl (mkTcOcc "VarValueIO"))
        varFieldsIOTy   <-  fmap mkTyConTy . tcLookupTyCon
                        =<< lookupTypeOccRn (mkOrig modl (mkTcOcc "VarFieldsIO"))

        ioTyCon <- tcLookupTyCon ioTyConName

        -- Try to compile and load an expression for all methods of `DebugView`
        -- applied to the dictionary for the given Type (`needle_ty`)
        let debugValueMN  = mthdRdrName "debugValueIOWrapper"
            debugFieldsMN = mthdRdrName "debugFieldsIOWrapper"
            debugValueWrapperMT =
              mkVisFunTyMany needle_ty $
                mkTyConApp ioTyCon [mkListTy varValueIOTy]
            debugFieldsWrapperMT =
              mkVisFunTyMany needle_ty $
                mkTyConApp ioTyCon [mkListTy varFieldsIOTy]
        !debugValue_fval  <- compileAndLoadMthd debugValueMN  debugValueWrapperMT
        !debugFields_fval <- compileAndLoadMthd debugFieldsMN debugFieldsWrapperMT

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
          liftIO $ logMsg logger MCDump noSrcSpan $
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
-- E.g. @compileAndLoadMthd "debugValue" (<ty> -> VarValue)@ returns the
-- foreign value for an expression @debugValue@ applied to the dictionary for
-- the requested type.
compileAndLoadMthd :: RdrName -- ^ Name of method/name of function that takes dictionary
                   -> Type    -- ^ The final type of expr when funct is alredy applied to dict
                   -> TcM ForeignHValue
compileAndLoadMthd mthName mthTy = do
  hsc_env <- getTopEnv

  let expr = nlHsVar mthName

  -- Rn, Tc, desugar applied to DebugView dictionary
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
