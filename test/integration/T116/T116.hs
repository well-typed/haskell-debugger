module Main where

import Data.IntMap
import qualified Data.IntMap as IM

main = do
  nn (IM.fromList [(0,345),(1,34),(46,345)])
  nn (IM.fromList [(0,1)])
  nn (IM.fromList [(0,2), (2,4)])
  nn (IM.fromList [(0,3)])

nn :: IntMap Int -> IO ()
nn im = do
  if False
    then return ()
    else do
      nnn im
      return ()

nnn :: IntMap Int -> IO ()
nnn im = do
  const (return ()) im
