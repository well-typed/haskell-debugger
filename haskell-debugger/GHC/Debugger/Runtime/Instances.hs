{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments #-}
module GHC.Debugger.Runtime.Instances where

import GHC
import GHC.Driver.Env
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import Control.Monad.Reader

import GHC.Debugger.Monad
import GHC.Debugger.Runtime.Instances.Discover
import GHC.Debugger.Runtime.TermParser

data VarValueResult = VarValueResult { varValueResult :: String, varValueResultExpandable :: Bool }

--------------------------------------------------------------------------------
-- * High level interface for 'DebugView' on 'Term's
--------------------------------------------------------------------------------

-- | Get the custom representation of this 'Term' by applying a 'DebugView'
-- instance 'debugValue' method if there is one.
debugValueTerm :: Term -> Debugger (Maybe VarValueResult)
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

            obtainParsedTerm "VarValue" maxBound True varValueIOTy transformed_v varValueParser >>= \case
              Left _ ->
                return Nothing
              Right (strTerm, valBool) -> do
                valStr <- liftIO $
                  evalString interp (val strTerm {- whose type is IO String, from varValueIO -})

                return $ Just VarValueResult
                  { varValueResult = valStr
                  , varValueResultExpandable = valBool
                  }



-- | Get the custom representation of this 'Term' by applying a 'DebugView'
-- instance 'debugFields' method if there is one.
--
-- Returns the mappings from field labels to terms, where each term records the
-- type and pointer to the foreign heap value returned in the instance for that label.
--
-- Returns @Nothing@ if no instance was found for the type of the given term
debugFieldsTerm :: Term -> Debugger (Maybe [(String, Term)])
debugFieldsTerm term = do
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

            obtainParsedTerm "VarFields" 2 True varFieldsIOTy transformed_v varFieldsParser >>= \case
              Left _ -> pure Nothing
              Right res -> pure (Just res)
