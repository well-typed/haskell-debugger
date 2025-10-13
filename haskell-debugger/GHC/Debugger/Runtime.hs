{-# LANGUAGE OrPatterns, GADTs, LambdaCase, NamedFieldPuns #-}
module GHC.Debugger.Runtime where

import Data.IORef
import Control.Monad.Reader
import qualified Data.List as L

import GHC
import GHC.Types.FieldLabel
import GHC.Tc.Utils.TcType
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect

import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Runtime.Term.Cache
import GHC.Debugger.Monad

-- | Obtain the runtime 'Term' from a 'TermKey'.
--
-- The 'TermKey' will be looked up in the 'TermCache' to avoid recomputing the
-- 'Term' if possible. On a cache miss the Term will be reconstructed from
-- scratch and stored in the cache.
obtainTerm :: TermKey -> Debugger Term
obtainTerm key = do
  hsc_env <- getSession
  tc_ref  <- asks termCache
  tc      <- liftIO $ readIORef tc_ref
  case lookupTermCache key tc of
    -- cache miss: reconstruct, then store.
    Nothing ->
      let
        -- For boring types we want to get the value as it is (by traversing it to
        -- the end), rather than stopping short and returning a suspension (e.g.
        -- for the string tail), because boring types are printed whole rather than
        -- being represented by an expandable structure.
        depth i = if isBoringTy (GHC.idType i) then maxBound else defaultDepth

        -- Recursively get terms until we hit the desired key.
        getTerm = \case
          FromId i -> GHC.obtainTermFromId (depth i) False{-don't force-} i
          FromPath k pf -> do
            term <- getTerm k
            liftIO $ expandTerm hsc_env $ case term of
              Term{dc=Right dc, subTerms} -> case pf of
                PositionalIndex ix -> subTerms !! (ix-1)
                LabeledField fl    ->
                  case L.findIndex (== fl) (map flSelector $ dataConFieldLabels dc) of
                    Just ix -> subTerms !! ix
                    Nothing -> error "Couldn't find labeled field in dataConFieldLabels"
              NewtypeWrap{wrapped_term} ->
                wrapped_term -- regardless of PathFragment
              _ -> error "Unexpected term for the given TermKey"
       in do
        term <- getTerm key
        liftIO $ modifyIORef tc_ref (insertTermCache key term)
        return term

    -- cache hit
    Just hit -> return hit

-- | Before returning a 'Term' we want to expand its heap representation up to the 'defaultDepth'
--
-- For 'Id's, this is done by 'GHC.obtainTermFromId'. For other 'TermKey's this
-- function should be used
expandTerm :: HscEnv -> Term -> IO Term
expandTerm hsc_env term = case term of
  Term{val, ty} -> cvObtainTerm hsc_env defaultDepth False ty val
  (NewtypeWrap{}; RefWrap{}) -> do
    -- TODO: we don't do anything clever here yet
    return term
  -- For other terms there's no point in trying to expand
  (Suspension{}; Prim{}) -> return term

-- | A boring type is one for which we don't care about the structure and would
-- rather see "whole" when being inspected. Strings and literals are a good
-- example, because it's more useful to see the string value than it is to see
-- a linked list of characters where each has to be forced individually.
isBoringTy :: Type -> Bool
isBoringTy t = isDoubleTy t || isFloatTy t || isIntTy t || isWordTy t || isStringTy t
                || isIntegerTy t || isNaturalTy t || isCharTy t


