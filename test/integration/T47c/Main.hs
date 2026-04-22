module Main where
import GHC.Debugger.View.Class
-- Look! I don't have to import the orphan instances!
-- import GHC.Debugger.View.Containers

import qualified Data.IntMap as IM

main :: IO ()
main = f (IM.fromList [(3,"one"), (2,"two")])

f :: Show a => a -> IO ()
f action = do
    print action

