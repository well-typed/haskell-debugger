module T325a where

import Control.Concurrent
import Control.Monad
import Control.Exception
import System.IO

main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Started"
  forever (threadDelay 1_000_000)
    `finally` putStrLn "debuggee shutting down"
