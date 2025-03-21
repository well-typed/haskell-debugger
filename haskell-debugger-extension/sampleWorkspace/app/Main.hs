{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text as T
import System.Environment
import Control.Exception
import Hi

abc :: Integer
abc = 456 ^ 345

data MyData = DataCon String

main :: IO ()
main = do
  args <- getArgs
  let str = f "Hello, Haskell!" "test"
    in putStrLn str
  hi
  fail "hello" `catch` \(x :: SomeException) ->
    putStrLn $ "All is OK\n" ++ unlines args ++ show x
  putStrLn "Goodbye!"

f :: String -> String -> String
f x y = x ++ y
