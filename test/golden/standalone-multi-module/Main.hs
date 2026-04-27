module Main where

import Helper

main :: IO ()
main = do
  putStrLn "Starting..."
  greet "world"
  putStrLn "Done."
