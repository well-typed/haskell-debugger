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
import GHC.Debugger.Interface.Messages
import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import Development.Debug.Adapter.Exit.Helpers

-- FIXME: difference between terminate and disconnect??

-- | Command terminate (1a)
--
-- Terminate the debuggee gracefully
commandTerminate :: DebugAdaptor ()
commandTerminate = do
  -- Terminate debuggee and sends acknowledgment.
  -- TODO: Terminate event instead of destroy session?
  -- DidTerminate <- sendInterleaved TerminateProcess sendTerminateResponse
  destroyDebugSession
  sendTerminateResponse
  terminateSessionCleanly Nothing

-- | Command disconnect (1b)
--
-- Terminate the debuggee (and any child processes) forcefully.
commandDisconnect :: DebugAdaptor ()
commandDisconnect = do
  destroyDebugSession
  sendDisconnectResponse
  terminateSessionCleanly Nothing
