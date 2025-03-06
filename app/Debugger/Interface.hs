module Debugger.Interface
  (
    -- * Client communication workers
    receiver, sender

    -- * Re-exports
  , module Debugger.Interface.Messages
  ) where

import Control.Concurrent
import Control.Monad

import Debugger.Interface.Messages

-- | Listens for 'Request' messages, deserializes them, and writes them to the given 'Chan', forever
--
-- Currently assumes
receiver :: Chan Request -> IO ()
receiver requests = forever $ do
  _

-- | Reads 'Response' messages from the given 'Chan', serializes them, and sends them to the other end, forever
sender :: Chan Response -> IO ()
sender replies = forever $ do
  _
