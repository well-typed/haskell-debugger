module Main where

import Hi

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  hi
  putStrLn "Goodbye!"
