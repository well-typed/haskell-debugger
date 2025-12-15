-- | A compilation cache which allows us to re-use an already loaded
-- expression/variable rather than recompiling it from scratch.
module GHC.Debugger.Runtime.Compile.Cache
  ( CompCache
  , insertCompRaw
  , lookupCompRaw
  , emptyCompCache
  ) where

import GHC

import qualified Data.Map as M

-- | Mapping from compile expressions (as strings) to their @ForeignHValue@s
newtype CompCache = CompCache (M.Map String ForeignHValue)

-- | Store the result of compiling and loading a raw expression string.
--
-- Note: it is easy to wrongly cache arbitrary compilation strings. You
-- shouldn't do this lightly since these arbitrary strings typically are not
-- very cacheable (there won't be many if any hits).
--
-- **Instead, one should always prefer to use @'RemoteExpr'@ to describe and load
-- remote expression, which will be implicitly cached but at a much finer
-- grained level.**
insertCompRaw :: String -> ForeignHValue -> CompCache -> CompCache
insertCompRaw s v (CompCache m) = CompCache (M.insert s v m)

-- | Look if the raw string value of an expression has already been compiled and loaded
lookupCompRaw :: String -> CompCache -> Maybe ForeignHValue
lookupCompRaw s (CompCache m) = M.lookup s m

-- | Nothing in it
emptyCompCache :: CompCache
emptyCompCache = CompCache M.empty
