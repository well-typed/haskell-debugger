module Main where
-- Look! No imports but still custom views for text!

import qualified Data.Text as T

main :: IO ()
main = f (T.pack "this should be displayed as a simple string")

f :: Show a => a -> IO ()
f action = do
    print action

