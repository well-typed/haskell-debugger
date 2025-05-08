{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Exception
import System.Environment
main :: IO ()
main = do
  args <- getArgs
  putStrLn "hello"
  print args
  fail "CATCH ME" `catch` (\(_::SomeException) -> putStrLn "caught it")
  putStrLn "goodbye"
  putStrLn (f 2 4 "call_fxxx")

f :: Int -> Int -> String -> String
f a b c = show (a + b) <> " " <> c
