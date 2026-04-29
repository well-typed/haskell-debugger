module Main where
-- IntMap with lazy thunk values: the thunk lives in the real data
-- structure, so forcing it persists across debugFields calls.

import qualified Data.IntMap.Lazy as IM

main :: IO ()
main = f (IM.fromList [(1, sum [1 .. 100 :: Int]), (2, sum [1 .. 200 :: Int])])

f :: Show a => a -> IO ()
f action = do
    print action
