module MyType
  ( MyType(..)
  , mkValue
  ) where

newtype MyType = MyType [String]
  deriving Show

mkValue :: MyType
mkValue = MyType ["alpha", "beta", "gamma"]
{-# OPAQUE mkValue #-}
