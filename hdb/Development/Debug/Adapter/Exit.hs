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

import Control.Monad.Except
import Control.Monad.IO.Class
import DAP
import Development.Debug.Adapter

-- | Command terminate (1a)
--
-- Terminate the *debuggee* gracefully
commandTerminate :: DebugAdaptor ()
commandTerminate = do
  destroyDebugSession -- kills debugger GHC session (which handles stopping the debuggee ext-interp too)
  sendTerminateResponse
  sendTerminatedEvent (TerminatedEvent False) -- we're done debugging now!

-- | Command disconnect (1b)
--
-- Terminate the debuggee (and any child processes) forcefully.
commandDisconnect :: DebugAdaptor ()
commandDisconnect = do
  -- kills debugger GHC session (which handles stopping the debuggee ext-interp too)
  -- ignore error if session has already been destroyed (e.g. client sends disconnect after terminate)
  destroyDebugSession `catchError` \ e -> liftIO $ putStrLn ("terminateSessionCleanly: ignoring missing session: " ++ show e)
  sendDisconnectResponse
  sendTerminatedEvent (TerminatedEvent False) -- we're done debugging now!
