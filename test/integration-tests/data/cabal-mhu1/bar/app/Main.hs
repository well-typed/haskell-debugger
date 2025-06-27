module Main where

import Bar
import Foo

main :: IO ()
main = do
  myFibonacciTest
  print (fib 5)
