{-# LANGUAGE OverloadedLists, TypeFamilies #-}
import GHC.IsList

data R = R { field :: Int }
  deriving Show

instance IsList R where
    type Item R = Int
    fromList [] = R 0
    fromList (x:_) = R x
    toList (R x) = [x]

f x = "f " ++ const x (show ([1,2,3] :: R))

main :: IO ()
main = do
    lcl <- pure "local"
    pure ()
    pure ()
    pure ()
    putStrLn (f lcl)
    putStrLn "goodbye"
