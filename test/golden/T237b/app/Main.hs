module Main (main) where

import Data.List (intercalate)

import GHC.Debugger.View.Class
import MyType

instance DebugView MyType where
  debugValue (MyType xs) = simpleValue (intercalate ":" xs) False
  debugFields _ = pure (VarFields [])

main :: IO ()
main = do
  let value = mkValue
  const (print value) value
