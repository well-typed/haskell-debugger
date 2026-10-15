module Main where

-- Newton's method for square roots, with a subtle bug:
-- the tolerance is absolute, so large inputs never converge
-- because of floating-point spacing.
newtonSqrt :: Double -> Double
newtonSqrt x = go x
  where
    go guess
      | abs (guess * guess - x) < 1e-10 = guess
      | otherwise =
          go ((guess + x / guess) / 2)   -- breakpoint, hit count: >= 100

main :: IO ()
main = do
  print (newtonSqrt 2)       -- fine, ~5 iterations
  print (newtonSqrt 1e6)     -- fine
  print (newtonSqrt 1e30)    -- hangs
