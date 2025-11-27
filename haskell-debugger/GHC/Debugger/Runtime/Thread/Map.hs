-- | A map to track and manage the debuggee runtime threads
module GHC.Debugger.Runtime.Thread.Map
  ( ThreadMap
  , emptyThreadMap

  -- * Operations
  , insertThreadMap
  , lookupThreadMap

  , threadMapToList

  -- can we detect when a thread has died? what happens if we have a reference
  -- to a ThreadId which has been GC'd?
  ) where

import Control.Concurrent
import Data.Coerce

import GHCi.RemoteTypes
import qualified Data.IntMap as IM

-- | A thread map maintains a mapping between the int thread identifier, which
-- uniquely identifies a thread spawned by the debuggee, and the (possibly
-- remote) reference to the thread (i.e. the corresponding ThreadId)
type ThreadMap = IM.IntMap (ForeignRef ThreadId)

-- | Insert a remote 'ThreadId' at this unique Int thread identifier
insertThreadMap :: Int -> ForeignRef ThreadId -> ThreadMap -> ThreadMap
insertThreadMap = IM.insert

-- | Lookup a remote 'ThreadId' by its unique Int identifier
lookupThreadMap :: Int -> ThreadMap -> Maybe (ForeignRef ThreadId)
lookupThreadMap = IM.lookup

-- | > It's empty, what did you expect?
emptyThreadMap :: ThreadMap
emptyThreadMap = IM.empty

-- | Get all the remote thread references from the ThreadMap
threadMapToList :: ThreadMap -> [(Int, ForeignRef ThreadId)]
threadMapToList = coerce . IM.toList

