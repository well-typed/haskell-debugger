module T325a where

import Control.Concurrent
import Control.Monad

main = do
  putStrLn "Started"
  forever $ threadDelay 1_000_000
