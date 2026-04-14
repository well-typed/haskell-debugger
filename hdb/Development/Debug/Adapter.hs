{-# LANGUAGE RecordWildCards #-}
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
import Network.Socket (PortNumber)

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
      { syncRequests  :: MVar D.Command
      , syncResponses :: MVar D.Response
      , nextFreshId   :: !Int
      , breakpointMap :: Map.Map GHC.InternalBreakpointId BreakpointSet
      , stackFrameMap :: IM.IntMap StackFrameIx
      , variablesMap  :: IM.IntMap VariablesIx
      , entryFile     :: FilePath
      , entryPoint    :: String
      , entryArgs     :: [String]
      , projectRoot   :: FilePath
      , runInTerminalProc :: RunInTerminalProc
        -- ^ Potentially a process launched via 'runInTerminal'.
      }

type BreakpointId = Int
type BreakpointSet = IS.IntSet

data StackFrameIx = StackFrameIx D.RemoteThreadId Int{-stack frame ix-}
  deriving (Eq, Ord)
data VariablesIx = VariablesIx StackFrameIx D.VariableReference

instance MonadFail DebugAdaptor where
  fail a = sendError (ErrorMessage (T.pack a)) Nothing

--------------------------------------------------------------------------------
-- * Run in terminal
--------------------------------------------------------------------------------

-- | A process launched via 'runInTerminal', which is attached to a user's terminal
data RunInTerminalProc
  -- | We're not using 'runInTerminal', so we didn't request the DAP client to
  -- launch a process on the user's terminal.
  = NoRunInTerminal

  -- | Instead of launching the external interpreter ourselves, we requested
  -- the DAP client to launch the external interpreter process
  | RunExternalInterpreterInTerminal
      { extInterpPort :: PortNumber
        -- ^ The port on which the external interpreter is running.
        --
        -- The external interpreter is only connected to a port when we launch
        -- it ourselves through 'runInTerminal'.
        --
        -- When 'NoRunInTerminal' but using external-interpreter, we'll still
        -- launch the external interpreter but in the default GHC way using the
        -- file descriptors directly.
      }

  -- | We launched @hdb proxy ...@ on the user's terminal.
  --
  -- This process will forward all input the user types into the
  -- debugger/debuggee process and be forwarded the debuggee's output to print.
  --
  -- We should always prefer to launch 'RunExternalInterpreterInTerminal'
  -- directly, but is not possible if using the internal interpreter.
  --
  -- See 'Development.Debug.Adapter.Proxy' for more details on @hdb proxy@.
  | RunProxyInTerminal
      { syncProxyIn :: Chan BS.ByteString
        -- ^ Read input to the debuggee from the proxy
      , syncProxyOut :: Chan BS.ByteString
        -- ^ Write output from the debuggee to the proxy
      , syncProxyErr :: Chan BS.ByteString
        -- ^ Write stderr from the debuggee to the proxy
      , proxyClientReady :: MVar ()
        -- ^ Wait for the runInTerminal proxy client to connect to the proxy
        -- server (#95).
        --
        -- Prevent a race where the debug session finishes before the
        -- 'runInTerminal' proxy has a chance to connect, so when it does
        -- finally try to connect it crashes.
      }

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
