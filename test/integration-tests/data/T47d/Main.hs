module Main where
-- Look! No imports but still custom views for containers!

import qualified Data.IntMap as IM

main :: IO ()
main = f (IM.fromList [(3,"one"), (2,"two")])

f :: Show a => a -> IO ()
f action = do
    print action

