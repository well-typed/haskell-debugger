{-# LANGUAGE LambdaCase #-}

-- | Module concerning with reporting failures and exiting cleanly the
-- debugging process. An overview of covered exit modes:
--
-- == 1. The top-level DebugAdaptor process
-- * Command Terminate
-- * Command Disconnect
-- * DebugAdaptor crashes while executing (handled by DAP library?)
-- * One of the threads launched by registerNewDebugSession crash
--
-- == 2. The haskell-debugger process
-- * The debugger crashes while initializing (e.g. while compiling or when discovering flags)
-- * The debugger crashes while executing a request
--
-- == 3. The debuggee loaded in haskell-debugger and runs
-- * The debuggee terminates successfully
-- * The debuggee terminates with an exception
-- * The debuggee crashes in another way
--
-- Notes:
-- * @'destroyDebugSession'@ kills all threads started for this session with @'registerNewDebugSession'@.
module Development.Debug.Adapter.Exit where

import DAP
import Data.Function
import System.IO
import Control.Monad.IO.Class
import Control.Exception
import Control.Exception.Context
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Development.Debug.Adapter
import qualified Development.Debug.Adapter.Output as Output

-- | Command terminate (1a)
--
-- Terminate the debuggee gracefully.
commandTerminate :: DebugAdaptor ()
commandTerminate = do
  -- Terminate debuggee and sends acknowledgment.
  -- TODO: Terminate event instead of destroy session?
  -- DidTerminate <- sendInterleaved TerminateProcess sendTerminateResponse
  destroyDebugSession
  sendTerminateResponse
  exitCleanly

-- | Command disconnect (1b)
--
-- Terminate the debuggee (and any child processes) forcefully.
commandDisconnect :: DebugAdaptor ()
commandDisconnect = do
  destroyDebugSession
  sendDisconnectResponse
  exitCleanly

--- Exit Cleanly ---------------------------------------------------------------

-- | Outputs a message notification ('Output.important'), sends a terminated
-- event, destroys the debug session, and dies.
--
-- ::WARNING::
--
-- This function should not be called if the debugsession with the debugger
-- threads haven't yet been registered because it WILL block on the call to
-- @'destroyDebugSession'@.
exitCleanupWithMsg
  :: Handle
  -- ^ Handle to finalize reading as OutputEvents before exiting (but after
  -- killing the output thread with @destroyDebugSession@)
  -> String
  -- ^ Error message, logged with notification
  -> DebugAdaptor a
exitCleanupWithMsg final_handle msg = do
  destroyDebugSession -- kill all session threads (including the output thread)
  do                  -- flush buffer and get all pending output from GHC
    c <- T.hGetContents final_handle & liftIO
    Output.neutral c
  exitWithMsg msg

-- | Logs the error to the debug console and sends a terminate event
exitWithMsg :: String -> DebugAdaptor a
exitWithMsg msg = do
  Output.important (T.pack msg)
  exitCleanly

exitCleanly :: DebugAdaptor a
exitCleanly = do

  sendTerminatedEvent (TerminatedEvent False)

  -- We exit here to guarantee the process is killed when
  -- terminated. Important! We want a new server process per
  -- session, which means at the end we must kill the server.
  liftIO $ throwIO TerminateServer

--- Utils ----------------------------------------------------------------------

-- | Display an exception with its context
displayExceptionWithContext :: SomeException -> String
displayExceptionWithContext ex = do
  case displayExceptionContext (someExceptionContext ex) of
    "" -> displayException ex
    cx -> displayException ex ++ "\n\n" ++ cx


