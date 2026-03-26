{-# OPTIONS_GHC -O -fno-unoptimized-core-for-interpreter #-}
{-# LANGUAGE BangPatterns #-}

module Main where

data Packed = Packed -- Unpacking only happens with optimizations, hence the -O.
  { packedLeft :: {-# UNPACK #-} !Char
  , packedRight :: {-# UNPACK #-} !Char
  }
  deriving Show

data Container = Container
  { containerBefore :: Int
  , containerPacked :: {-# UNPACK #-} !Packed
  , containerPacked2 :: {-# UNPACK #-} !Packed
  , containerAfter :: Int
  }
  deriving Show

mkContainer :: Container
mkContainer = Container 10 (Packed 'a' 'b') (Packed 'c' 'd') 40
{-# OPAQUE mkContainer #-}

main :: IO ()
main = do
  let !container = mkContainer
  const (pure ()) container
  print container
