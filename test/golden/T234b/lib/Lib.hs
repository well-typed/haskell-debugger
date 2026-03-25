module Lib (mainish) where

import Data.IORef (IORef, newIORef)
import GHC.Debugger.View.Class

newtype ColonList = ColonList (IORef [String])

instance Show ColonList where
  show _ = "<colon-list>"

instance DebugView ColonList where
  debugValue _ = simpleValue "This is an IORef!" False
  debugFields _ = pure (VarFields [])

mainish :: IO ()
mainish = do
  value <- mkValue
  print value

mkValue :: IO ColonList
mkValue = ColonList <$> newIORef ["alpha", "beta", "gamma"]
