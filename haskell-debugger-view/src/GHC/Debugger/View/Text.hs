{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Debugger.View.Text where

import GHC.Debugger.View.Class

import qualified Data.Text          as T

instance DebugView T.Text where
  debugValue  t = VarValue (show (T.unpack t)) False
  debugFields _ = VarFields []

