{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Exception
import System.Environment
main :: IO ()
main = do
  args <- getArgs
  putStrLn "hello"
  print args
  throwIO (ErrorCall "CATCH ME") `catch` (\(_::SomeException) -> putStrLn "caught it")
  putStrLn "goodbye"
  return ()
