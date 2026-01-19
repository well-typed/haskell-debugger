{-# LANGUAGE OrPatterns, GADTs, LambdaCase, NamedFieldPuns, TemplateHaskellQuotes #-}
module GHC.Debugger.Runtime where

import Control.Monad.Reader
import qualified Data.List as L

import GHC
import GHC.Utils.Outputable
import GHC.Types.FieldLabel
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect

import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Monad

-- | Obtain the runtime 'Term' from a 'TermKey'.
--
-- The 'TermKey' will be looked up in the 'TermCache' to avoid recomputing the
-- 'Term' if possible. On a cache miss the Term will be reconstructed from
-- scratch and stored in the cache.
obtainTerm :: TermKey -> Debugger Term
obtainTerm key = do
  hsc_env <- getSession
   -- Recursively get terms until we hit the desired key.
  case key of
     FromId i -> GHC.obtainTermFromId defaultDepth False{-don't force-} i
     FromPath k pf -> do
       term <- obtainTerm k
       liftIO $ expandTerm hsc_env $ case term of
         Term{dc=Right dc, subTerms} -> case pf of
           PositionalIndex ix -> subTerms !! (ix-1)
           LabeledField fl    ->
             case L.findIndex (== fl) (map flSelector $ dataConFieldLabels dc) of
               Just ix -> subTerms !! ix
               Nothing -> error "Couldn't find labeled field in dataConFieldLabels"
         NewtypeWrap{wrapped_term} ->
           wrapped_term -- regardless of PathFragment
         RefWrap{wrapped_term} ->
           wrapped_term -- regardless of PathFragment
         _ -> error ("Unexpected term for the given TermKey because <term> should have been expanded before and we're getting a path fragment!\n" ++ showPprUnsafe (ppr key <+> ppr k <+> ppr pf))
     FromCustomPath _key _name ctm -> do
       -- For custom terms return them straightaway.
       liftIO $ expandTerm hsc_env ctm
     FromCustomRoot _name ctm -> do
       liftIO $ expandTerm hsc_env ctm
     -- import GHC.Builtin.Types.Prim (alphaTyVar, alphaTy)
     -- FromHValRoot _name val -> do
     --   -- Use (forall a. a) polymorphic type to refine
     --   let polyTy = mkForAllTy (mkTyVarBinder Specified alphaTyVar) alphaTy
     --   liftIO $ cvObtainTerm hsc_env defaultDepth False polyTy val


-- | Before returning a 'Term' we want to expand its heap representation up to the 'defaultDepth'
--
-- For 'Id's, this is done by 'GHC.obtainTermFromId'. For other 'TermKey's this
-- function should be used
expandTerm :: HscEnv -> Term -> IO Term
expandTerm hsc_env term = case term of
  Term{val, ty} -> cvObtainTerm hsc_env defaultDepth False ty val
  RefWrap{wrapped_term} -> do
    wt' <- expandTerm hsc_env wrapped_term
    return term{wrapped_term=wt'}
  NewtypeWrap{wrapped_term} -> do
    wt' <- expandTerm hsc_env wrapped_term
    return term{wrapped_term=wt'}
  Suspension{val, ty} -> cvObtainTerm hsc_env defaultDepth False ty val
  Prim{} -> return term

