{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import Control.Exception
import Hi

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Hello, Haskell!"
  hi
  fail "hello" `catch` \(_ :: SomeException) ->
    putStrLn $ "All is OK\n" ++ unlines args
  putStrLn "Goodbye!"
