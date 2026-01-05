{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
module Lib where

import GHC.Debugger.View.Class

data MyType = MyType deriving Show
deriving via (BoringTy MyType) instance (DebugView MyType)

my :: MyType
my = MyType

