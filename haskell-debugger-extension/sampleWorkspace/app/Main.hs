{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text as T
import System.Environment
import Control.Exception
import Hi

abc :: Integer
abc = 456 ^ 345

data MyData = DataCon MyFields
newtype Test = Test MyData

main :: IO ()
main = do
  args <- getArgs
  let str = f "Hello, Haskell!" "test"
  let str' = g $ Test $ DataCon (MyFls (fst str) (snd str))
  hi
  putStrLn str'
  fail "hello" `catch` \(x :: SomeException) ->
    putStrLn $ "All is OK\n" ++ unlines args ++ show x
  putStrLn "Goodbye!"

data MyFields = MyFls { abcdef :: String, abcdefg :: String }

f :: String -> String -> (String, String)
f x y = (x, y)
{-# OPAQUE f #-}

g :: Test -> String
g a = case a of
  Test (DataCon x) -> abcdef x ++ abcdefg x
