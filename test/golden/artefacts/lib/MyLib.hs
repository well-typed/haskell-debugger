module MyLib where

foo :: String -> IO ()
foo x = putStrLn (x ++ " got added")
