{-# LANGUAGE GADTs, ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Word

data Foo where
  Foo :: Show a => a -> Foo

data Foo2 where
  Foo2 :: Show a => { foo :: a }-> Foo2

data Foo3 where
  Foo3 :: Int -> {-# UNPACK #-} !Word -> !Int -> !Int -> Foo3

data Foo4 = Foo4 !Word32 !Word16 !Word64 !Word16

instance Show Foo where
  show (Foo a) = show a

data Foo5 where
  Foo5 :: (?impl :: String) => String -> Foo5

mkFoo :: Foo
mkFoo = Foo $! ("value" :: String)

mkFoo2 :: Foo2
mkFoo2 = Foo2 $! ("value2" :: String)

mkFoo5 :: Foo5
mkFoo5 = let ?impl = "bob" in Foo5 "alice"


inspectFoo :: IO ()
inspectFoo = do
  let !existentialFoo = mkFoo
  let !existentialFoo2 = mkFoo2
  let !foo3 = Foo3 0 1 2 3
  let !foo4 = Foo4 0 1 2 3
  let !foo5 = mkFoo5
  const (pure ()) (existentialFoo, existentialFoo2, foo3, foo4, foo5)
  case existentialFoo of
    Foo inner -> putStrLn (show inner)

main :: IO ()
main = inspectFoo
