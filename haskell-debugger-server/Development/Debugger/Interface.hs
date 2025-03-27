{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Development.Debugger.Interface where

import qualified Data.Text as T
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import DAP

import Debugger.Interface.Messages as D
import Development.Debugger.Adaptor
import qualified Development.Debugger.Output as Output

-- | Synchronously send a command to the debugger and await a response
sendSync :: D.Command -> DebugAdaptor Response
sendSync cmd = do
  DAS{..} <- getDebugSession
  liftIO $ putMVar syncRequests cmd
  liftIO (takeMVar syncResponses) >>= handleAbort

-- | Sends a command to the debugger, then runs the given action, and only after running the action it waits for the result of the debugger
sendInterleaved :: D.Command -> DebugAdaptor () -> DebugAdaptor Response
sendInterleaved cmd action = do
  DAS{..} <- getDebugSession
  liftIO $ putMVar syncRequests cmd
  () <- action
  liftIO (takeMVar syncResponses) >>= handleAbort

handleAbort :: Response -> DebugAdaptor Response
handleAbort (Aborted e) = do
  Output.console (T.pack e)
  sendTerminatedEvent defaultTerminatedEvent
  return (Aborted e) -- will TerminatedEvent terminate this session before this happens?
handleAbort r = return r

