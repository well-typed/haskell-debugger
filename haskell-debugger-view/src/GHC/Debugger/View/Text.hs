{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Debugger.View.Text where

import GHC.Debugger.View.Class

import qualified Data.Text          as T

instance DebugView T.Text where
  debugValue  t = simpleValue (show (T.unpack t)) False
  debugFields _ = pure $ VarFields []

