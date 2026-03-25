module Lib (mainish) where

import Data.List (intercalate)
import GHC.Debugger.View.Class

data ColonList = ColonList [String]
  deriving Show

instance DebugView ColonList where
  debugValue (ColonList xs) = simpleValue (intercalate ":" xs) False
  debugFields _ = pure (VarFields [])

mainish :: IO ()
mainish = do
  let value = mkValue
  print value

mkValue :: ColonList
mkValue = ColonList ["alpha", "beta", "gamma"]
