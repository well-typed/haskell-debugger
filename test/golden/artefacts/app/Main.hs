module Main (main) where

import MyLib

main :: IO ()
main = do
  let x = "some var"
  foo x
  putStrLn x
