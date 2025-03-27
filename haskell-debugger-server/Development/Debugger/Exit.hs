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
-- == 2. The ghc-debugger process
-- * GHC debugger crashes while initializing (e.g. while compiling or when discovering flags)
-- * GHC debugger crashes while executing a request
--
-- == 3. The debuggee loaded in ghc-debugger and runs
-- * The debuggee terminates successfully
-- * The debuggee terminates with an exception
-- * The debuggee crashes in another way
--
-- Notes:
-- * @'destroyDebugSession'@ kills all threads started for this session with @'registerNewDebugSession'@.
module Development.Debugger.Exit where

import DAP
import System.IO
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Exception
import Control.Exception.Context
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Debugger.Interface.Messages
import Development.Debugger.Adaptor
import qualified Development.Debugger.Output as Output

-- | Command terminate (1a)
--
-- Terminate the debuggee gracefully.
commandTerminate :: DebugAdaptor ()
commandTerminate = do
  -- Terminate debuggee and sends acknowledgment.
  --
  -- TODO:
  -- Is the session terminated, or does it keep going (and can be re-used with a new DebugExecution request)?
  -- I suspect we from here on use a new `sessionId` so we should instead kill
  -- the session after it says DidTerminate too because nothing is re-used.
  --
  -- TODO: Terminate event instead of destroy session
  -- DidTerminate <- sendInterleaved TerminateProcess sendTerminateResponse
  destroyDebugSession
  sendTerminateResponse
  sendTerminatedEvent (TerminatedEvent True{-can restart this session?!-})

-- | Command disconnect (1b)
--
-- Terminate the debuggee (and any child processes) forcefully.
commandDisconnect :: DebugAdaptor ()
commandDisconnect = do
  destroyDebugSession -- does not send terminated event because thread catches ThreadKilled exception.
  sendDisconnectResponse
  sendTerminatedEvent (TerminatedEvent False)

-- | Handle a crash of a debugger session thread (1d)
--
-- This will also handle a session thread being killed by
-- @'destroyDebugSession'@ and do nothing in that case.
--
-- TODO: Revise. Currently unused.
-- handleDebuggerException :: Handle {-^ The handle from which we read the debugger output -}
--                         -> SomeException
--                         -> DebugAdaptor ()
-- handleDebuggerException readOut e | Just ThreadKilled <- fromException e = do
--   return () -- terminated event is sent by killer
-- handleDebuggerException readOut e = do
--   Output.console $ T.pack ("Caught: " <> displayExceptionWithContext e)
--   sendTerminatedEvent (TerminatedEvent False)

--- Exit Cleanly ---------------------------------------------------------------

-- | Replies with a "canceled" 'ErrorResponse', outputs a message notification
-- ('Output.important'), sends a terminated event, destroys the debug
-- session, and dies.
exitCleanlyWithErrorResponse :: String -- ^ Error response message, logged with notification
                             -> DebugAdaptorX r
exitCleanlyWithErrorResponse msg = do
  sendError ErrorMessageCancelled Nothing
  Output.important (T.pack msg)
  -- sendTerminatedEvent (TerminatedEvent False) TODO: CANNOT SEND WITHOUT 'REQUEST' DebugAdaptor
  destroyDebugSession -- kill all session threads
  -- exitWith (ExitFailure 1)


--- Utils ----------------------------------------------------------------------

-- | Display an exception with its context
displayExceptionWithContext :: SomeException -> String
displayExceptionWithContext ex = do
  case displayExceptionContext (someExceptionContext ex) of
    "" -> displayException ex
    cx -> displayException ex ++ "\n\n" ++ cx


