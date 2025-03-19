{-# LANGUAGE RecordWildCards #-}
module Development.Debugger.Interface where

import Control.Concurrent.MVar
import Control.Monad.IO.Class

import DAP

import Debugger.Interface.Messages as D
import Development.Debugger.Adaptor

-- | Synchronously send a command to the debugger and await a response
sendSync :: D.Command -> DebugAdaptor Response
sendSync cmd = do
  DAS{..} <- getDebugSession
  liftIO $ do
    putMVar syncRequests cmd
    takeMVar syncResponses

-- | Sends a command to the debugger, then runs the given action, and only after running the action it waits for the result of the debugger
sendInterleaved :: D.Command -> DebugAdaptor () -> DebugAdaptor Response
sendInterleaved cmd action = do
  DAS{..} <- getDebugSession
  liftIO $ putMVar syncRequests cmd
  () <- action
  liftIO $ takeMVar syncResponses

