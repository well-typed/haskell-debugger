module Main where

data X = X (String, Double)
    deriving Show

main :: IO ()
main = f (X ("A33", 3456))

f :: Show a => a -> IO ()
f action = do
    print action
