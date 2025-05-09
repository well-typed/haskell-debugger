{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Stopped.Variables where

import Control.Monad
import Control.Monad.IO.Class

import GHC
import GHC.Types.FieldLabel
import GHC.Runtime.Eval
import GHC.Core.DataCon
import GHC.Types.Id as GHC
import GHC.Tc.Utils.TcType
import GHC.Utils.Misc (zipEqual)
import GHC.Types.Unique.Supply (uniqFromTag)
import qualified GHC.Runtime.Debugger as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Utils

-- | 'TyThing' to 'VarInfo'. The 'Bool' argument indicates whether to force the
-- value of the thing (as in @True = :force@, @False = :print@)
tyThingToVarInfo :: Int {-^ Depth -} -> TyThing -> Debugger VarInfo
tyThingToVarInfo depth0 = \case
  t@(AConLike c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables <*> pure NoFields
  t@(ATyCon c)   -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables <*> pure NoFields
  t@(ACoAxiom c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables <*> pure NoFields
  AnId i -> do
    -- For boring types we want to get the value as it is (by traversing it to
    -- the end), rather than stopping short and returning a suspension (e.g.
    -- for the string tail), because boring types are printed whole rather than
    -- being represented by an expandable structure.
    let depth1 = if isBoringTy (GHC.idType i) then maxBound else depth0
    term <- GHC.obtainTermFromId depth1 False{-don't force-} i
    termToVarInfo (GHC.idName i) term

-- | Construct a 'VarInfo' from the given 'Name' of the variable and the 'Term' it binds
termToVarInfo :: Name -> Term -> Debugger VarInfo
termToVarInfo top_name top_term = do

  -- Make a VarInfo for the top term.
  top_vi <- go top_name top_term

  sub_vis <- case top_term of
      -- Boring types don't get subfields
      _ | isBoringTy (GHCI.termType top_term) ->
        return NoFields

      -- Make 'VarInfo's for the first layer of subTerms only.
      Term{dc=Right dc, subTerms} -> do
        case dataConFieldLabels dc of
          -- Not a record type,
          -- Use indexed fields
          [] -> do
            names <- zipWithM (\ix _ -> mkPositionalVarFieldName top_name ix) [1..] (dataConOrigArgTys dc)
            IndexedFields <$> mapM (\(n',t') -> go n' t') (zipEqual names subTerms)
          -- Is a record type,
          -- Use field labels
          dataConFields -> do
            let names = map flSelector dataConFields
            LabeledFields <$> mapM (uncurry go) (zipEqual names subTerms)
      NewtypeWrap{dc=Right dc, wrapped_term} -> do
        case dataConFieldLabels dc of
          [] -> do
            name <- mkPositionalVarFieldName top_name 1
            wvi <- go name wrapped_term
            return (IndexedFields [wvi])
          [fld] -> do
            let name = flSelector fld
            wvi <- go name wrapped_term
            return (LabeledFields [wvi])
          _ -> error "unexpected number of Newtype fields: larger than 1"
      _ -> return NoFields

  return top_vi{varFields = sub_vis}

  where
    -- Make a VarInfo for a term, but don't recurse into the fields and return
    -- @NoFields@ for 'varFields'.
    --
    -- We do this because we don't want to recursively return all sub-fields --
    -- only the first layer of fields for the top term.
    go n term = do
      let
        varFields = NoFields
        isThunk
          -- to have more information we could match further on @Heap.ClosureType@
         | Suspension{} <- term = True
         | otherwise = False
        ty = GHCI.termType term

        -- We scrape the subterms to display as the var's value. The structure is
        -- displayed in the editor itself by expanding the variable sub-fields
        -- (`varFields`). 
        termHead t
          -- But show strings and lits in full
          | isBoringTy ty = t
          | otherwise     = case t of
             Term{}                    -> t{subTerms = []}
             NewtypeWrap{wrapped_term} -> t{wrapped_term = termHead wrapped_term}
             _                         -> t
      varName <- display n
      varType <- display ty
      varValue <- display =<< GHCD.showTerm (termHead term)
      -- liftIO $ print (varName, varType, varValue, GHCI.isFullyEvaluatedTerm term)

      -- The VarReference allows user to expand variable structure and inspect its value.
      -- Here, we do not want to allow expanding a term that is fully evaluated.
      -- We only want to return @SpecificVariable@ (which allows expansion) for
      -- values with sub-fields or thunks.
      varRef <- do
        if GHCI.isFullyEvaluatedTerm term
           -- Even if it is already evaluated, we do want to display a
           -- structure as long if it is not a "boring type" (one that does not
           -- provide useful information from being expanded)
           -- (e.g. consider how awkward it is to expand Char# 10 and I# 20)
           && (isBoringTy ty || not (hasDirectSubTerms term))
         then
            return NoVariables
         else do
            ir <- insertVarReference n term
            return (SpecificVariable ir)

      return VarInfo{..}

    hasDirectSubTerms = \case
      Suspension{}   -> False
      Prim{}         -> False
      NewtypeWrap{}  -> True
      RefWrap{}      -> True
      Term{subTerms} -> not $ null subTerms

-- | A boring type is one for which we don't care about the structure and would
-- rather see "whole" when being inspected. Strings and literals are a good
-- example, because it's more useful to see the string value than it is to see
-- a linked list of characters where each has to be forced individually.
isBoringTy :: Type -> Bool
isBoringTy t = isDoubleTy t || isFloatTy t || isIntTy t || isWordTy t || isStringTy t
                || isIntegerTy t || isNaturalTy t || isCharTy t


