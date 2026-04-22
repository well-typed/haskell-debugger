module Main where

data T = T !Int !Int
  deriving Show

main = do
  y (T 333 34)

y :: T -> IO ()
y t = do
  print t
