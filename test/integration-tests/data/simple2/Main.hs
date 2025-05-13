module Main where

import System.Environment

data Pair = Pair (String, String) String

data Depth = Depth String Depth | OK

main = do
  args <- getArgs
  putStrLn $ showPair (Pair ("abcdasdf","ef") (show args))
  putStrLn $ showDepth (Depth "d=1" (Depth "d=2" (Depth "d=3" (Depth "d=4" (Depth "d=5" (Depth "d=6" OK))))))

showPair p = case p of
  Pair x y ->
    case x of
      (a, b) -> a ++ b ++ y

showDepth p = case p of
  OK -> "OK"
  Depth s d -> s ++ ";" ++ showDepth d

