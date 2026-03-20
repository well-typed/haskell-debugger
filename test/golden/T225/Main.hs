{-# LANGUAGE OverloadedLists, GeneralizedNewtypeDeriving, DerivingStrategies, UndecidableInstances #-}
module Main where

import GHC.IsList

newtype List a = List [a]
  deriving Show
  deriving newtype IsList

main = do
  return ()
  return ()
