module Main where

import System.Environment

main = do
    args <- getArgs
    putStrLn "hi"
    putStrLn $ "bye " ++ show args
    fail "bad"


f x =
    let y = x ^ 3
        z = y ^ 4
     in z + y

