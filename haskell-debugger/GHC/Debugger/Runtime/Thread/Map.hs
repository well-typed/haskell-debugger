-- | A map to track and manage the debuggee runtime threads
module GHC.Debugger.Runtime.Thread.Map
  ( ThreadMap
  , emptyThreadMap

  -- * Operations
  , insertThreadMap
  , lookupThreadMap
  , deleteThreadMap

  , threadMapToList

  -- can we detect when a thread has died? what happens if we have a reference
  -- to a ThreadId which has been GC'd?
  ) where

import Data.Coerce

import qualified Data.IntMap as IM

-- | A thread map maintains a mapping between the int thread identifier, which
-- uniquely identifies a thread spawned by the debuggee, and the (possibly
-- remote) reference to the thread (i.e. the corresponding ThreadId)
type ThreadMap a = IM.IntMap a

-- | Insert a remote 'ThreadId' at this unique Int thread identifier
insertThreadMap :: Int -> a -> ThreadMap a -> ThreadMap a
insertThreadMap = IM.insert

-- | Lookup a remote 'ThreadId' by its unique Int identifier
lookupThreadMap :: Int -> ThreadMap a -> Maybe a
lookupThreadMap = IM.lookup

-- | Insert a remote 'ThreadId' at this unique Int thread identifier
deleteThreadMap :: Int -> ThreadMap a -> ThreadMap a
deleteThreadMap = IM.delete

-- | > It's empty, what did you expect?
emptyThreadMap :: ThreadMap a
emptyThreadMap = IM.empty

-- | Get all the remote thread references from the ThreadMap
threadMapToList :: ThreadMap a -> [(Int, a)]
threadMapToList = coerce . IM.toList

