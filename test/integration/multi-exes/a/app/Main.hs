module Main where

main :: IO ()
main = do
  let y = sum [1..60]
  let x = fib y
      z = fib 500
  putStrLn $ "Hello, A!" ++ show (length (show x)) ++ show (z)

fib :: Int -> Integer
fib n = fibs !! n

fibs = 1: 1: zipWith (+) fibs (tail fibs)

