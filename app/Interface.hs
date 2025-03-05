module Interface where

import Control.Concurrent
import Control.Monad

data Request

data Response

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
