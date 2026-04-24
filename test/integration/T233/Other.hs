module Other where

process :: [Int] -> IO ()
process xs = do
  let n = length xs
  print n
