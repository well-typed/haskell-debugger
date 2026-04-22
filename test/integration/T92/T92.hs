module Main where

import Data.IORef

main = do
  r <- newIORef False
  putStrLn (const "hi" r) -- make r free in this line
  writeIORef r True
  putStrLn (const "bye" r) -- what does r look like now?

