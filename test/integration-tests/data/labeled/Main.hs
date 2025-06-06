module Main where

data Something = S { new :: Int, lab :: String }
    deriving Show
main :: IO ()
main = do
    let x = f ()
    print x

f :: () -> Something
{-# OPAQUE f #-}
f () = S { new = 3456, lab = "label" }

