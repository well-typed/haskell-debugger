{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import Control.Exception
import Hi

main :: IO ()
main = do
  args <- getArgs
  let str = f "Hello, Haskell!" "test"
    in putStrLn str
  hi
  fail "hello" `catch` \(_ :: SomeException) ->
    putStrLn $ "All is OK\n" ++ unlines args
  putStrLn "Goodbye!"

f :: String -> String -> String
f x y = x ++ y
