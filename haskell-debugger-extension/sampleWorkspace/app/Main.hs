{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text as T
import System.Environment
import Control.Exception
import Hi

abc :: Integer
abc = 456 ^ 345

data MyData = DataCon MyFields
newtype Test2 = Test2 { hasFieldName :: MyData }
newtype Test = Test Test2

main :: IO ()
main = do
  args <- getArgs
  let str = f "Hello, Haskell!" "test"
  let str' = g $ Test $ Test2 $ DataCon (MyFls (fst str) (snd str))
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
  Test y -> case y of
    Test2 z -> case z of
      DataCon x -> abcdef x ++ abcdefg x
