module Main where

import Data.List (foldl')

-- Simulated sensor readings with one glitch buried in the middle
readings :: [Int]
readings = [ if i == 6173 then -999 else (i * 37) `mod` 100 | i <- [1 .. 10000] ]

data Stats = Stats { total :: !Int, count :: !Int } deriving Show

step :: Stats -> Int -> Stats
step (Stats t c) r =
  Stats (t + r) (c + 1)          -- breakpoint here, condition: r < 0

main :: IO ()
main = do
  let Stats t c = foldl' step (Stats 0 0) readings
  putStrLn $ "Average: " ++ show (fromIntegral t / fromIntegral c :: Double)
