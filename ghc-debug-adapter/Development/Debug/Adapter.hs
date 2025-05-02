module Development.Debug.Adapter where

import Control.Concurrent.MVar
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Text as T
import System.FilePath

import qualified GHC
import DAP
import qualified Debugger.Interface.Messages as D (Command, Response)

type DebugAdaptor = Adaptor DebugAdaptorState Request
type DebugAdaptorCont = Adaptor DebugAdaptorState ()
type DebugAdaptorX r = Adaptor DebugAdaptorState r ()

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

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- | Transform the given file into a DAP 'Source'. The file may be modified:
--
--    * if the given filepath is absolute, it's returned unchanged
--    * if it is relative to the project root, it's returned absolute (with the project root prepended)
fileToSource :: FilePath -> DebugAdaptor Source
fileToSource file = do
  root <- projectRoot <$> getDebugSession
  fullPath <- if isAbsolute file
     then return file
     else return (root </> file)
  return defaultSource{sourcePath = Just (T.pack fullPath)}

