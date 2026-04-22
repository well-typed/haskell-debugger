module Main where

main :: IO ()
main = do
  case foo False undefined of
    1 ->  putStrLn "one"
    2 ->  putStrLn "two"

foo = \b x -> if b then 1 else 2

