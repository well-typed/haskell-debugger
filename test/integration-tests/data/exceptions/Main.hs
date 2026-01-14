module Main where

import Control.Exception

main :: IO ()
main = do
  putStrLn "About to throw outer exception"
  catch throwOuter handler
  putStrLn "unreachable"

throwOuter :: IO ()
throwOuter = error "outer boom"

handler :: SomeException -> IO ()
handler se = do
  putStrLn ("Handling exception: " ++ displayException se)
  error "inner boom"
