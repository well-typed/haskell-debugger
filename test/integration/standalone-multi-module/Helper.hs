module Helper where

greet :: String -> IO ()
greet name = do
  putStrLn ("Hello, " ++ name ++ "!")
