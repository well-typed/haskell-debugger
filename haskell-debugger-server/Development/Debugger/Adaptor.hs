module Development.Debugger.Adaptor where

import Control.Concurrent.MVar
import qualified Data.IntSet as IS
import qualified Data.Map as Map

import qualified GHC
import DAP
import qualified Debugger.Interface.Messages as D (Command, Response)

type DebugAdaptor = Adaptor DebugAdaptorState

-- | Debugger state:
--
-- * Keep a mapping from DAP breakpoint ids to internal breakpoint ids
-- * Keep the MVar through which synchronous communication with the debugger is done.
--    - The debugger main worker writes to this MVar responses (and, for now, events too)
--    - The handler worker reads from this MVar and writes them to the client with the 'Adapter'.
data DebugAdaptorState = DAS
      { syncRequests :: MVar D.Command
      , syncResponses :: MVar D.Response
      , nextFreshBreakpointId :: !BreakpointId
      , breakpointMap :: Map.Map GHC.BreakpointId BreakpointSet
      , entryPoint :: String
      , entryArgs :: [String]
      , projectRoot :: FilePath
      }

type BreakpointId = Int
type BreakpointSet = IS.IntSet

instance MonadFail DebugAdaptor where
  fail a = error a
  -- TODO: PROPER ERROR HANDLING with termination, possibly delete this instance
