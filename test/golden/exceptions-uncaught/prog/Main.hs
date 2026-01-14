module Main where

import Control.Exception

main :: IO ()
main = do
  putStrLn "About to throw"
  catch (error "boom outer") handler
  putStrLn "Should not reach here"

handler :: SomeException -> IO ()
handler se = do
  putStrLn ("Handling exception: " ++ displayException se)
  error "boom while handling"
