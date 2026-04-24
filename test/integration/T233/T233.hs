module Main where

import Data.List (sort, nub)
import qualified Data.Map.Strict as Map
import Other (process)

main :: IO ()
main = do
  let xs = [3, 1, 4, 1, 5, 9, 2, 6] :: [Int]
  let m = Map.fromList [("a", 1), ("b", 2)] :: Map.Map String Int
  check xs m
  process xs

check :: [Int] -> Map.Map String Int -> IO ()
check xs m = do
  print xs
  print m
