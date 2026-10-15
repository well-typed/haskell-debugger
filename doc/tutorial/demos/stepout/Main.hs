module Main where

data Order = Order { qty :: Int, prices :: [Int] }

-- validate :: Order -> Order
-- validate o
--   | qty o <= 0         = error "bad quantity"
--   | any (< 0) (prices o) = error "negative price"
--   | null (prices o)    = error "empty order"
--   | otherwise          = o

validate :: Order -> Order
validate o = case o of
  _ | qty o <= 0           -> error "bad quantity"
    | any (< 0) (prices o) -> error "negative price"
    | null (prices o)      -> error "empty order"
    | otherwise            -> o

applyDiscount :: Int -> Int -> Int
applyDiscount q subtotal
  | q >= 3    = subtotal - subtotal `div` 10
  | otherwise = subtotal

orderTotal :: Order -> Int
orderTotal o =
  let v = validate o                              -- breakpoint here
  in applyDiscount (qty v) (sum (prices v))

main :: IO ()
main = print (orderTotal (Order 3 [10, 20, 30]))
