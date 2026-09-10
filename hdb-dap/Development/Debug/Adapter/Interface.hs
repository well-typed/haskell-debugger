{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Development.Debug.Adapter.Interface where

import qualified Data.Text as T
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import DAP

import GHC.Debugger.Interface.Messages as D
import Development.Debug.Adapter
import qualified Development.Debug.Adapter.Output as Output

-- | Synchronously send a command to the debugger and await a response
sendSync :: D.Command -> DebugAdaptor Response
sendSync cmd = do
  DAS{..} <- getDebugSession
  liftIO $ putMVar syncRequests cmd
  liftIO (takeMVar syncResponses) >>= handleErrors

-- | Sends a command to the debugger, then runs the given action, and only after running the action it waits for the result of the debugger
sendInterleaved :: D.Command -> DebugAdaptor () -> DebugAdaptor Response
sendInterleaved cmd action = do
  DAS{..} <- getDebugSession
  liftIO $ putMVar syncRequests cmd
  () <- action
  liftIO (takeMVar syncResponses) >>= handleErrors

handleErrors :: Response -> DebugAdaptor Response
handleErrors (NonFatalError e) = do
  Output.console (T.pack e)
  -- reply still in this connection with "ErrorResponse" to pending request
  sendError (ErrorMessage (T.pack e)) Nothing
handleErrors (Aborted e) = do
  Output.console (T.pack e)
  sendTerminatedEvent (TerminatedEvent False)
  destroyDebugSession -- kill this debug session's threads
  -- reply still in this connection with "ErrorResponse" to pending request
  sendError (ErrorMessage (T.pack e)) Nothing
handleErrors r = return r
