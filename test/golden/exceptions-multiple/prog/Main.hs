module Main where

import Control.Exception

main :: IO ()
main = do
  putStrLn "About to throw first"
  catch (error "boom first") handler
  putStrLn "Continuing after first exception"
  error "boom second"

handler :: SomeException -> IO ()
handler se = putStrLn ("Handled first exception: " ++ displayException se)
