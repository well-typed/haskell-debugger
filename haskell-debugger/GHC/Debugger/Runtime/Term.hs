{-# LANGUAGE OrPatterns, GADTs, LambdaCase, NamedFieldPuns, TemplateHaskellQuotes #-}
module GHC.Debugger.Runtime.Term
  ( obtainTerm
  , seqTerm
  , deepseqTerm
  ) where

import Control.Monad.Reader

import GHC
import GHC.Utils.Outputable
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect

import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Monad
import GHC.Plugins
import qualified GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Interpreter (fromEvalResult)

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
         Term{dc=Right _, subTerms} -> case pf of
           PositionalIndex ix -> subTerms !! (ix-1)
           LabeledField n _ -> subTerms !! (n-1)
         NewtypeWrap{wrapped_term} ->
           wrapped_term -- regardless of PathFragment
         RefWrap{wrapped_term} ->
           wrapped_term -- regardless of PathFragment
         _ -> error ("Unexpected term for the given TermKey because <term> should have been expanded before and we're getting a path fragment!\n" ++ showPprUnsafe (ppr key <+> ppr k <+> ppr pf))
     FromCustomTerm _key _name ctm -> do
       -- For custom terms return them straightaway.
       liftIO $ expandTerm hsc_env ctm


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

--------------------------------------------------------------------------------
-- * Forcing laziness
--------------------------------------------------------------------------------

-- | The depth determines how much of the runtime structure is traversed.
-- @obtainTerm@ and friends handle fetching arbitrarily nested data structures
-- so we only depth enough to get to the next level of subterms.
defaultDepth :: Int
defaultDepth =  2

-- | Evaluate a suspended Term to WHNF.
--
-- Used in @'getVariables'@ to reply to a variable introspection request.
seqTerm :: HscEnv -> Term -> IO Term
seqTerm hsc_env term = do
  let
    interp = hscInterp hsc_env
    unit_env = hsc_unit_env hsc_env
  case term of
    Suspension{val, ty} -> do
#if MIN_VERSION_ghc(9,15,0)
      r <- GHCi.seqHValue interp unit_env (hsc_logger hsc_env) val
#else
      r <- GHCi.seqHValue interp unit_env val
#endif
      () <- fromEvalResult r
      let
        forceThunks = False {- whether to force the thunk subterms -}
        forceDepth  = defaultDepth
      cvObtainTerm hsc_env forceDepth forceThunks ty val
    NewtypeWrap{wrapped_term} -> do
      wrapped_term' <- seqTerm hsc_env wrapped_term
      return term{wrapped_term=wrapped_term'}
    _ -> return term

-- | Evaluate a Term to NF
deepseqTerm :: HscEnv -> Term -> IO Term
deepseqTerm hsc_env t = case t of
  Suspension{}   -> do t' <- seqTerm hsc_env t
                       deepseqTerm hsc_env t'
  Term{subTerms} -> do subTerms' <- mapM (deepseqTerm hsc_env) subTerms
                       return t{subTerms = subTerms'}
  NewtypeWrap{wrapped_term}
                 -> do wrapped_term' <- deepseqTerm hsc_env wrapped_term
                       return t{wrapped_term = wrapped_term'}
  _              -> do seqTerm hsc_env t
