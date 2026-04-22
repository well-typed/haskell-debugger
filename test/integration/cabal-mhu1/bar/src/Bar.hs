module Bar where

import Foo

myFibonacciTest :: IO ()
myFibonacciTest = do
  print (fib 1)
  print (fib 4)
  print (fib 8)
  print (fib 10)
