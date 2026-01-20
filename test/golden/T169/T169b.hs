module Main where

import System.IO

main :: IO ()
main = do
  hPutStrLn stdout "Debuggee writing something to stdout"
  hPutStrLn stderr "Debuggee writing something to stderr"
