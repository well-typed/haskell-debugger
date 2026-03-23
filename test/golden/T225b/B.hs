{-# LANGUAGE OverloadedLists #-}
module B where

import Data.Set qualified as Set

ex :: Set.Set Int
ex = [1,2,3]

f = do
  print 1
  got <- return ex
  print got
  pure ()
