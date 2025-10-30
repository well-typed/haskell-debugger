{-# LANGUAGE NamedFieldPuns, DeriveFunctor, DerivingStrategies, GeneralizedNewtypeDeriving #-}
-- | Meant to be qualified with @import qualified GHC.Debugger.Breakpoint.Map as BM@
module GHC.Debugger.Breakpoint.Map
  ( BreakpointMap
  , insert
  , lookup
  , delete
  , empty
  , lookupModuleIBIs
  , keys
  , toList
  ) where

import Prelude hiding (lookup)
import qualified GHC
import GHC.Unit.Module.Env
import GHC.ByteCode.Breakpoints
import GHC.Utils.Outputable (Outputable)
import qualified Data.IntMap as IM

-- | A map keyed by 'InternalBreakpointId'
newtype BreakpointMap a = BreakpointMap (ModuleEnv (IM.IntMap a))
  deriving newtype Outputable

insert :: GHC.InternalBreakpointId -> a -> BreakpointMap a -> BreakpointMap a
insert InternalBreakpointId{ibi_info_mod, ibi_info_index}
        x (BreakpointMap bm0) = BreakpointMap $
  case lookupModuleEnv bm0 ibi_info_mod of
    Nothing ->
      extendModuleEnv bm0 ibi_info_mod $
        IM.singleton ibi_info_index x
    Just im ->
      extendModuleEnv bm0 ibi_info_mod $
        IM.insert ibi_info_index x im

lookup :: GHC.InternalBreakpointId -> BreakpointMap a -> Maybe a
lookup InternalBreakpointId{ibi_info_mod, ibi_info_index}
        (BreakpointMap bm0) = do
  lookupModuleEnv bm0 ibi_info_mod
  >>= IM.lookup ibi_info_index

delete :: GHC.InternalBreakpointId -> BreakpointMap a -> BreakpointMap a
delete InternalBreakpointId{ibi_info_mod, ibi_info_index}
        (BreakpointMap bm0) =
  case lookupModuleEnv bm0 ibi_info_mod of
    Nothing -> BreakpointMap bm0
    Just im -> BreakpointMap $
      extendModuleEnv bm0 ibi_info_mod $
        IM.delete ibi_info_index im

empty :: BreakpointMap a
empty = BreakpointMap emptyModuleEnv

-- | Retrieves all 'InternalBreakpointId's for a given 'Module'
-- Note: The internal breakpoints of a module are not necessarily the same as
-- the source-level breakpoints, so this shouldn't be used to get all internal
-- breakpoints with source-level occurrences in the given module.
lookupModuleIBIs :: GHC.Module -> BreakpointMap a -> [InternalBreakpointId]
lookupModuleIBIs m (BreakpointMap bm) =
  case lookupModuleEnv bm m of
    Nothing -> []
    Just im ->
      [ InternalBreakpointId m bix
      | bix <- IM.keys im
      ]

keys :: BreakpointMap a -> [InternalBreakpointId]
keys (BreakpointMap bm) =
  [ InternalBreakpointId m bix
  | (m, im) <- moduleEnvToList bm
  , bix <- IM.keys im
  ]

toList :: BreakpointMap a -> [(InternalBreakpointId, a)]
toList (BreakpointMap bm) =
  [ (InternalBreakpointId m bix, a)
  | (m, im)  <- moduleEnvToList bm
  , (bix, a) <- IM.toList im
  ]
