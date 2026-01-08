module Development.Debug.Adapter where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Data.IntSet as IS
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.Text as T
import System.FilePath

import DAP
import qualified GHC
import qualified GHC.Debugger.Interface.Messages as D (Command, Response, RemoteThreadId, VariableReference)

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
      , nextFreshId :: !Int
      , breakpointMap :: Map.Map GHC.InternalBreakpointId BreakpointSet
      , stackFrameMap :: IM.IntMap StackFrameIx
      , variablesMap  :: IM.IntMap VariablesIx
      , entryFile :: FilePath
      , entryPoint :: String
      , entryArgs :: [String]
      , projectRoot :: FilePath
      , syncProxyIn :: Chan BS.ByteString
        -- ^ Read input to the debuggee from the proxy
      , syncProxyOut :: Chan BS.ByteString
        -- ^ Write output from the debuggee to the proxy
      , syncProxyErr :: Chan BS.ByteString
        -- ^ Write stderr from the debuggee to the proxy
      }

type BreakpointId = Int
type BreakpointSet = IS.IntSet

data StackFrameIx = StackFrameIx D.RemoteThreadId Int{-stack frame ix-}
  deriving (Eq, Ord)
data VariablesIx = VariablesIx StackFrameIx D.VariableReference

instance MonadFail DebugAdaptor where
  fail a = error a
  -- TODO: PROPER ERROR HANDLING with termination, possibly delete this instance
  -- use: exitWithMsg.

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

-- | Generate fresh Int identifier.
getFreshId :: DebugAdaptor Int
getFreshId = do
  nid <- nextFreshId <$> getDebugSession
  updateDebugSession $ \s -> s { nextFreshId = nextFreshId s + 1 }
  pure nid
