import System.Environment
main :: IO ()
main = do
  args <- getArgs
  putStrLn "hello"
  print args
  putStrLn "goodbye"
  return ()
