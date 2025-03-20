{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception
import Hi

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  hi
  fail "hello" `catch` \(_ :: SomeException) ->
    putStrLn "all oK"
  putStrLn "Goodbye!"
