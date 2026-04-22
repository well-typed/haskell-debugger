import System.IO

main :: IO ()
main = do
    putStrLn "hello"
    arg <- getLine
    print arg
    putStrLn "goodbye"
