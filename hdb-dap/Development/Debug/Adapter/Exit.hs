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
import Development.Debug.Adapter
import Development.Debug.Adapter.Interface (sendSync)
import GHC.Debugger.Interface.Messages (Command(TerminateDebuggee), Response (DidTerminate))
import Control.Monad (when)
import Control.Monad.Catch

-- | Command terminate (1a)
--
-- Terminate the *debuggee* gracefully
commandTerminate :: DebugAdaptor ()
commandTerminate = do
  DidTerminate b <- sendSync TerminateDebuggee
  when b $ do
    safeDestroyDebugSession
    sendTerminatedEvent (TerminatedEvent False) -- we're done debugging now!
  -- the response only acknowledges the command,
  -- does not imply successful termination.
  sendTerminateResponse

-- | Command disconnect (1b)
--
-- Terminate the debuggee (and any child processes) forcefully.
commandDisconnect :: DebugAdaptor ()
commandDisconnect = do
  -- kills debugger GHC session (which handles stopping the debuggee ext-interp too)
  -- ignore error if session has already been destroyed (e.g. client sends disconnect after terminate)
  safeDestroyDebugSession
  sendDisconnectResponse
  throwM DisconnectDAPClientCleanly
