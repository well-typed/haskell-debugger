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
import GHCi.Message

import GHC.Debugger.Monad
import GHC.Debugger.Session.Builtin
import GHC.Debugger.View.Class
import GHC.Debugger.Logger as Logger

import GHC.Debugger.Runtime.Instances.Discover

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
  mbInst <- getDebugViewInstance ty
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
  mbInst <- getDebugViewInstance ty
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

