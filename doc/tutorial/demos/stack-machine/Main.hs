module Main where

data Instr = Push Int | Add | Mul | Dup | Swap deriving Show

exec :: [Int] -> Instr -> [Int]
exec stack instr =
  case (instr, stack) of    -- logpoint: "{instr} on {stack}"
    (Push n, s)         -> n : s
    (Add, a : b : s)    -> a + b : s
    (Mul, a : b : s)    -> a * b : s
    (Dup, a : s)        -> a : a : s
    (Swap, a : b : s)   -> b : a : s
    _                   -> error ("stack underflow at " ++ show instr)

main :: IO ()
main = print (foldl exec [] [Push 3, Dup, Mul, Push 4, Swap, Add, Mul])
