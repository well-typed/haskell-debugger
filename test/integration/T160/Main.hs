module Main where

import X

main :: IO ()
main = do
    putStrLn "Testing stack frames:"
    print (h 1)
    print (function1 5)
    print (function2 "hello")

function1 :: Int -> String
function1 x =
    -- Call function2 as the case scrutinee and do extra processing so it's not a tail call
    case function2 (if x == 0 then "foo" else "hello") of
        "no" -> "function1 saw no"
        "yes" -> "function1 saw yes"
        other -> "f1-" ++ other

function2 :: String -> String
function2 s =
    -- Call function3 as the case scrutinee and transform its result
    case function3 (case s of
                        "hello" -> [1,2]
                        "foo" -> []
                        other -> [length other]) of
        "no" -> "function2-no"
        "yes" -> "function2-yes"
        other -> other ++ "-f2"
