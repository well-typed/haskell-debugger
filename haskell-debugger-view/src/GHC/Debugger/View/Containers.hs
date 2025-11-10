{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Debugger.View.Containers where

import GHC.Debugger.View.Class

import qualified Data.IntMap        as IM
import qualified Data.Map           as M

instance DebugView (IM.IntMap a) where
  debugValue _ = VarValue "IntMap" True
  debugFields im = VarFields
    [ (show k, VarFieldValue v)
    | (k, v) <- IM.toList im
    ]

instance Show k => DebugView (M.Map k a) where
  debugValue _ = VarValue "Map" True
  debugFields m = VarFields
    [ (show k, VarFieldValue v)
    | (k, v) <- M.toList m
    ]
