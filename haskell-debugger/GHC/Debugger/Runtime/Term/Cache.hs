{-# LANGUAGE GADTs, DataKinds #-}
module GHC.Debugger.Runtime.Term.Cache where

import GHC.Types.Var.Env

import GHC.Debugger.Runtime.Term.Key

import Data.Map (Map)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- * TermKeyMap
--------------------------------------------------------------------------------

-- | Mapping from 'TermKey' to @a@. Backs 'TermCache', but is more general.
type TermKeyMap a = IdEnv (Map [PathFragment True] a)

-- | Lookup a 'TermKey' in a 'TermKeyMap'.
lookupTermKeyMap :: TermKey -> TermKeyMap a -> Maybe a
lookupTermKeyMap key tc = do
  let (i, path) = unconsTermKey key
  path_map <- lookupVarEnv tc i
  M.lookup path path_map

-- | Inserts a 'Term' for the given 'TermKey' in the 'TermKeyMap'.
--
-- Overwrites existing values.
insertTermKeyMap :: TermKey -> a -> TermKeyMap a -> TermKeyMap a
insertTermKeyMap key term tc =
  let
    (i, path) = unconsTermKey key
    new_map = case lookupVarEnv tc i of
      Nothing           -> M.singleton path term
      Just existing_map -> M.insert path term existing_map
  in extendVarEnv tc i new_map
