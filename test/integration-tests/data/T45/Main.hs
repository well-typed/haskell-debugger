import System.Environment (getArgs)
main :: IO ()
main = do
    putStrLn "Going to read args "
    [arg] <- getArgs
    putStrLn $ "Success...: " ++ arg
