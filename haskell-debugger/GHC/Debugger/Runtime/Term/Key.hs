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
  FromPath :: TermKey -> PathFragment -> TermKey

  -- | Use a custom term, by custom name, along a TermKey path, rather than
  -- reconstructing one from the 'FromId' root.
  FromCustomPath :: TermKey -> String -> Term -> TermKey

  -- | Use a custom term, by custom name, as the root of the key
  FromCustomRoot :: String -> Term -> TermKey

-- | A term may be identified by an 'Id' (such as a local variable) plus a list
-- of 'PathFragment's to an arbitrarily nested field.
data PathFragment where

  -- | A positional index is an index from 1 to inf
  PositionalIndex :: Int -> PathFragment

  -- | A labeled field indexes a datacon fields by name
  LabeledField    :: Name -> PathFragment

  deriving (Eq, Ord)

instance Outputable TermKey where
  ppr (FromId i)             = ppr i
  ppr (FromPath _ last_p)    = ppr last_p
  ppr (FromCustomPath _ s _) = text s
  ppr (FromCustomRoot s _)   = text s

instance Outputable PathFragment where
  ppr (PositionalIndex i) = text "_" <> ppr i
  ppr (LabeledField n)    = ppr n

