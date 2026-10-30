module Main where

data Order = Order { qty :: Int, prices :: [Int] }

validate :: Order -> Order
validate o = case o of
  _ | qty o <= 0           -> error "bad quantity"
    | any (< 0) (prices o) -> error "negative price"
    | null (prices o)      -> error "empty order"
    | otherwise            -> o

applyDiscount :: Int -> Int -> Int
applyDiscount q subtotal =
  if q >= 3
    then subtotal - subtotal `div` 10
    else subtotal

orderTotal :: Order -> Int
orderTotal o =
  let v = validate o
  in applyDiscount (qty v) (sum (prices v))        -- breakpoint here

main :: IO ()
main = print (orderTotal (Order 3 [10,20.. 50]))


















































-- Script:
-- StepOver: (qty v)
-- StepInto: validate o
-- StepInto: case o of ..
-- StepOver: | qty o <= 0
-- StepOut: (qty v) again
-- StepOut: if q >= 3
-- StepOver: subtotal - subtotal `div` 10
-- StepInto: (sum (prices v))
-- StepInto: prices
-- StepInto: subtotal `div` 10
-- Any: done.
