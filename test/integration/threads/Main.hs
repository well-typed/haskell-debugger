module Main where

import Control.Concurrent
import Control.Monad
import GHC.Conc
import System.IO

worker :: Int -> IO ()
worker n = do
  putStrLn $ "Worker " ++ show n ++ " starting"
  threadDelay 500000         -- 0.5s
  putStrLn $ "Worker " ++ show n ++ " midpoint"
  threadDelay 700000         -- 0.7s
  putStrLn $ "Worker " ++ show n ++ " done"
  worker n

setLabel :: String -> IO ()
setLabel s = myThreadId >>= \tid -> labelThread tid s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Main: spawning workers"
  tids <- forM [1..4] $ \i -> forkIO (setLabel ("Worker " ++ show i) >> worker i)
  putStrLn $ "Main: spawned " ++ show (length tids) ++ " workers"
  forever $ threadDelay 1000000
