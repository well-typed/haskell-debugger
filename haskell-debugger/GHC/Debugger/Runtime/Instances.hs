{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, OrPatterns #-}
module GHC.Debugger.Runtime.Instances
  ( debugValueTerm
  , debugFieldsTerm
  , VarValueResult(..)
  ) where

import GHC
import GHC.Driver.Env
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import GHC.Utils.Outputable
import Control.Monad.Reader

import GHC.Debugger.Monad
import Colog.Core as Logger
import GHC.Debugger.Runtime.Instances.Discover
import GHC.Debugger.Runtime.Term.Parser

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
  case (,) <$> maybe_hval term <*> mbInst of
    Nothing -> return Nothing
    Just (hval, DebugViewInstance
      {instDebugValue, varValueIOTy}) -> do
        liftIO (instDebugValue hval) >>= \case
          Left _e ->
            -- exception! ignore.
            return Nothing
          Right transformed_v -> do

            obtainParsedTerm "VarValue" maxBound True varValueIOTy transformed_v varValueParser >>= \case
              Left _ ->
                return Nothing
              Right (strTerm, valBool) -> do
                case strTerm of
                  (Suspension{} ; Term{}) -> do
                    valStr <- liftIO $
                      evalString interp (val strTerm {- whose type is IO String, from varValueIO -})

                    return $ Just VarValueResult
                      { varValueResult = valStr
                      , varValueResultExpandable = valBool
                      }
                  _ -> do
                    logSDoc Logger.Warning (text "debugValueTerm(2): Expecting" <+> ppr strTerm <+> text "to be a Term or Suspension.")
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
  let ty = termType term
  mbInst <- getDebugViewInstance ty
  case (,) <$> maybe_hval term <*> mbInst of
    Nothing -> return Nothing
    Just (hval, DebugViewInstance
      {instDebugFields, varFieldsIOTy}) -> do
        liftIO (instDebugFields hval) >>= \case
          Left _e ->
            -- exception! ignore.
            return Nothing
          Right transformed_v -> do

            obtainParsedTerm "VarFields" 2 True varFieldsIOTy transformed_v varFieldsParser >>= \case
              Left _ -> pure Nothing
              Right res -> pure (Just res)

-- | The heap value of the Term the debugView instance methods are applied to
-- (looks through newtypes). For Primitive types and primitive References,
-- assume dbgInst methods can't be applied to them.
maybe_hval :: Term -> Maybe ForeignHValue
maybe_hval t = case t of
  Suspension{val} -> Just val
  Term{val}       -> Just val
  NewtypeWrap{wrapped_term}
                  -> maybe_hval wrapped_term
  Prim{}          -> Nothing
  RefWrap{}       -> Nothing
