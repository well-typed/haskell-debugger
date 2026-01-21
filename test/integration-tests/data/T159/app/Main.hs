module Main (main) where

import GHC.Stack.Annotation.Experimental

main :: IO ()
main = do
  annotateCallStackIO $ do
    annotateStackShowIO ([1..4] :: [Int]) $ do
      annotateStackStringIO "Lovely annotation" $ do
        foo 500

foo :: Int -> IO ()
foo arg =
  putStrLn $ "foo: " <> show (arg * arg * arg)
