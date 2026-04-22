module Main where

main :: IO ()
main = do
  let x = ackermann 3 3
  putStrLn $ "Hello, B! " ++ show (length (show x))

ackermann 0 m = m + 1
ackermann n 0 = ackermann (n - 1) 1
ackermann n m = ackermann (n - 1) (ackermann n (m - 1))
