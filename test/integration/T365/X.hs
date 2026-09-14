{-# LANGUAGE UnboxedTuples #-}
module X where

-- preserves var in continuation
-- also workaround for "bitmap of size 0" bug.
{-# OPAQUE keep #-}
keep :: a -> b -> b
keep x y = seq x y

function3 :: [Int] -> String
function3 xs =
    -- Call function4 as the case scrutinee and add non-tail processing
    case function4 (sum xs > 0) of
        "no" -> keep xs "function3-no"
        "yes" -> keep xs "function3-yes"
        other -> "function3-" ++ other

function4 :: Bool -> String
function4 b = do
    -- Use a case expression on the Bool
    case b of
        True -> keep b "yes"
        False -> "no"

g :: Int -> (# Int, Int, Int #)
g x =
  (# x, x + 1, x + 2 #)

h :: Int -> Int
h x = case g x of        -- alts frame expects an unboxed tuple
        (# a, b, _ #) -> a + b
