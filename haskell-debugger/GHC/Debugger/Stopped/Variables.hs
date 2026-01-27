{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns, DerivingVia,
   TypeAbstractions, DataKinds #-}
module GHC.Debugger.Stopped.Variables where

import Control.Monad.Reader

import GHC
import GHC.Types.FieldLabel
import GHC.Types.Id.Info
import GHC.Types.Var
import GHC.Runtime.Eval
import GHC.Core.DataCon
import GHC.Core.TyCo.Rep
import qualified GHC.Runtime.Debugger     as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime
import GHC.Debugger.Runtime.Instances
import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Utils

-- | 'TyThing' to 'VarInfo'. The 'Bool' argument indicates whether to force the
-- value of the thing (as in @True = :force@, @False = :print@)
tyThingToVarInfo :: TyThing -> Debugger VarInfo
tyThingToVarInfo t = case t of
  AConLike c -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  ATyCon c   -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  ACoAxiom c -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables
  AnId i
    | DataConWrapId data_con <- idDetails i
    -- Newtype cons don't have a runtime representation, so we can't obtain
    -- terms! Simply print the newtype cons like we do data cons.
    -- See Note [Newtype workers]
    , isNewTyCon (dataConTyCon data_con)
    -> VarInfo <$> display data_con <*> display t <*> display t <*> pure False <*> pure NoVariables
  AnId i -> do
    let key = FromId i
    term <- obtainTerm key
    termToVarInfo key term

-- | Construct the VarInfos of the fields ('VarFields') of the given 'TermKey'/'Term'
--
-- This is used to come up with terms for the fields of an already `seq`ed
-- variable which was expanded.
termVarFields :: TermKey -> Term -> Debugger VarFields
termVarFields top_key top_term = do

  vcVarFields <- debugFieldsTerm top_term

  case vcVarFields of
    -- The custom instance case (top_term should always be a @Term@ if @Just@)
    Just fls -> do

      let keys = map (\(f_name, f_term) -> FromCustomTerm top_key f_name f_term) fls
      VarFields <$> mapM (\k -> obtainTerm k >>= termToVarInfo k) keys

    -- The general case
    _ -> case top_term of
      -- Make 'VarInfo's for the first layer of subTerms only.
      Term{dc=Right dc, subTerms=_{- don't use directly! go through @obtainTerm@ -}} -> do
        case dataConFieldLabels dc of
          -- Not a record type,
          -- Use indexed fields
          [] -> do
            let keys = zipWith (\ix _ -> FromPath top_key (PositionalIndex ix)) [1..] (dataConRepArgTys dc)
            VarFields <$> mapM (\k -> obtainTerm k >>= termToVarInfo k) keys
          -- Is a record data con,
          -- Use field labels
          dataConFields -> do
            let mkPath ix Nothing = FromPath top_key (PositionalIndex ix)
                mkPath ix (Just fld) = FromPath top_key (LabeledField ix (flSelector fld))
                keys = zipWith mkPath [1..] ((Nothing <$ dataConOtherTheta dc) ++ (map Just dataConFields))
            VarFields <$> mapM (\k -> obtainTerm k >>= termToVarInfo k) keys
      NewtypeWrap{dc=Right dc, wrapped_term=_{- don't use directly! go through @obtainTerm@ -}} -> do
        case dataConFieldLabels dc of
          [] -> do
            let key = FromPath top_key (PositionalIndex 1)
            wvi <- obtainTerm key >>= termToVarInfo key
            return (VarFields [wvi])
          [fld] -> do
            let key = FromPath top_key (LabeledField 1 (flSelector fld))
            wvi <- obtainTerm key >>= termToVarInfo key
            return (VarFields [wvi])
          _ -> error "unexpected number of Newtype fields: larger than 1"
      RefWrap{wrapped_term=_{- don't use directly! go through @obtainTerm@ -}} -> do
        let key = FromPath top_key (PositionalIndex 1)
        wvi <- obtainTerm key >>= termToVarInfo key
        return (VarFields [wvi])
      _ -> return (VarFields [])


-- | Construct a 'VarInfo' from the given 'Name' of the variable and the 'Term' it binds
termToVarInfo :: TermKey -> Term -> Debugger VarInfo
termToVarInfo key term0 = do
  -- Make a VarInfo for a term
  let
    ty = GHCI.termType term0

    -- Check for function types explicitly since they seem to always match Suspension
    -- but should not be shown as thunks in the UI.
    checkFn (FunTy _ _ _ _) = True
    checkFn (ForAllTy _ t) = checkFn t
    checkFn _ = False
    isFn = checkFn ty

  varName <- display key
  varType <- display ty
  case term0 of
    -- The simple case: The term is a a thunk...
    Suspension{} -> do
      return VarInfo
        { varName
        , varType
        , varValue = if isFn
            then "<fn> :: " ++ varType
            else "_"
        , varRef = if isFn
            then NoVariables
            else SpecificVariable key -- allows forcing the thunk
        , isThunk = not isFn
        }

    -- Otherwise, try to apply and decode a custom 'DebugView', or default to
    -- the inspecting the original term generically
    _ -> do

      -- Try to apply `DebugView.debugValue`
      mterm <- debugValueTerm term0

      case mterm of
        -- Default to generic representation
        Nothing -> do

          let
            -- In the general case, scrape the subterms to display as the var's value.
            -- The structure is displayed in the editor itself by expanding the
            -- variable sub-fields
            termHead t = case t of
               Term{} -> t{subTerms = []}
               _      -> t

          varValue <- display =<< GHCD.showTerm (termHead term0)

          -- The VarReference allows user to expand variable structure and inspect its value.
          -- Here, we do not want to allow expanding a term that is fully evaluated.
          -- We only want to return @SpecificVariable@ (which allows expansion) for
          -- values with sub-fields or thunks.
          varRef <- do
            if hasDirectSubTerms term0
             then do
                return (SpecificVariable key)
             else do
                return NoVariables

          return VarInfo
            { varName, varType
            , isThunk = False
            , varValue, varRef }

        Just VarValueResult{varValueResultExpandable=varExpandable, varValueResult=value} -> do

          varRef <-
            if varExpandable
            then do
                return (SpecificVariable key)
             else do
                return NoVariables
          return VarInfo
            { varName, varType
            , isThunk = False
            , varValue = value
            , varRef
            }

  where
    hasDirectSubTerms = \case
      Suspension{}   -> False
      Prim{}         -> False
      NewtypeWrap{}  -> True
      RefWrap{}      -> True
      Term{subTerms} -> not $ null subTerms

-- | Forces a term to WHNF
--
-- The term is updated in the cache at the given key.
forceTerm :: Term -> Debugger Term
forceTerm term = do
  hsc_env <- getSession
  liftIO $ seqTerm hsc_env term

