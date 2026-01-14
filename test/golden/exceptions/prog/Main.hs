module Main where

main :: IO ()
main = do
  putStrLn "About to throw"
  error "boom"
