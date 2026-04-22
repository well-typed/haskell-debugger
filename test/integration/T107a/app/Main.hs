module Main (main) where

import MyLib


main :: IO ()
main = a `thenDo` ("before" ++ "OK ")
    where
        a x = do
            putStrLn x
            pure (x++"hi2b")
