module Main where

data T97 a = T97 a
  deriving Show


main = do
  let y :: T97 (T97 (T97 (T97 (T97 (T97 String)))))
      y = T97 (T97 (T97 (T97 (T97 (T97 "hello")))))

  print y
  print y

