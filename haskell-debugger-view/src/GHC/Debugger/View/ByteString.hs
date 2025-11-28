{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Debugger.View.ByteString where

import GHC.Debugger.View.Class

import qualified Data.ByteString    as BS

instance DebugView BS.ByteString where
  debugValue  t = simpleValue (show t) False
  debugFields _ = pure (VarFields [])

