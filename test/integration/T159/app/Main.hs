module Main (main) where

import GHC.Stack.Annotation.Experimental

main :: IO ()
main = do
  annotateCallStackIO $ do
    annotateStackShowIO ([1..4] :: [Int]) $ do
      annotateStackStringIO "Lovely annotation" $ do
        foo 500 10

{-# OPAQUE foo #-}
foo :: Int -> Int -> IO ()
foo arg arg2 =
  putStrLn $ "foo: " <> show (arg * arg * arg2)
