{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Debugger.Utils.Orphans where

import GHC.Debugger.View.Class
import GHC.Data.FastString

instance DebugView FastString where
  debugValue  t = simpleValue (unpackFS t) False
  debugFields _ = pure (VarFields [])
