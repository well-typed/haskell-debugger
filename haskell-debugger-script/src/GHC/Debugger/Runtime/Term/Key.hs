{-# LANGUAGE GADTs, ViewPatterns, DataKinds #-}
module GHC.Debugger.Runtime.Term.Key where

import Prelude hiding ((<>))

import GHC
import GHC.Utils.Outputable
import GHC.Runtime.Eval

-- | A 'TermKey' serves to fetch a Term in a Debugger session.
-- Note: A 'TermKey' is only valid in the stopped context it was created in.
data TermKey where
  -- | Obtain a term from an Id.
  FromId :: Id -> TermKey

  -- | Append a PathFragment to the current Term Key. Used to construct keys
  -- for indexed and labeled fields.
  FromPath :: TermKey -> PathFragment False -> TermKey

  -- | Use a custom term, by custom name, along a TermKey path, rather than
  -- reconstructing one from the 'FromId' root.
  FromCustomTerm :: TermKey -> String -> Term -> TermKey

-- | A term may be identified by an 'Id' (such as a local variable) plus a list
-- of 'PathFragment's to an arbitrarily nested field.
data PathFragment (b :: Bool {- whether allow custom field -}) where
  -- | A positional index is an index from 1 to inf
  PositionalIndex :: Int -> PathFragment b
  -- | A labeled field indexes a datacon fields by name
  -- The position is given by the 'Int'
  -- The name is cosmetic.
  LabeledField    :: Int -> Name -> PathFragment b
deriving instance Eq (PathFragment b)
deriving instance Ord (PathFragment b)

instance Outputable TermKey where
  ppr (FromId i)             = ppr i
  ppr (FromPath _ last_p)    = ppr last_p
  ppr (FromCustomTerm _ s _) = text s

instance Outputable (PathFragment b) where
  ppr (PositionalIndex i) = text "_" <> ppr i
  ppr (LabeledField _ n)    = ppr n

