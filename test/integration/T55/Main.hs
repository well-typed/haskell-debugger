module Main where

newtype MyInt = MyInt Int
    deriving (Show)

newtype MyIntX = MyIntX X
    deriving (Show)


data X = X MyInt
    deriving Show

newtype Y = Y MyInt
    deriving Show

newtype Y2 = Y2 MyIntX
    deriving Show

data R = R {_fieldOfR :: MyInt}
    deriving Show

main :: IO ()
main = do
    putStrLn $ show $ bpmi $ MyInt 42
    putStrLn $ show $ bpx $ X (MyInt 42)
    putStrLn $ show $ bpy $ Y (MyInt 42)
    putStrLn $ show $ bpy2 $ Y2 (MyIntX (X (MyInt 42)))
    putStrLn $ show $ bpr $ R (MyInt 42)

bpmi :: MyInt -> MyInt
bpmi mi = mi

bpx :: X -> X
bpx x = x

bpy :: Y -> Y
bpy y = y

bpy2 :: Y2 -> Y2
bpy2 y2 = y2

bpr :: R -> R 
bpr r = r
