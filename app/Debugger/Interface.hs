module Debugger.Interface
  (
    -- * Client communication workers
    receiver, sender

    -- * Re-exports
  , module Debugger.Interface.Messages
  ) where

import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO (stdin, stderr, hPutStrLn, Handle)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as Aeson

import Debugger.Interface.Messages

-- | Listens for 'Request' messages, deserializes them, and writes them to the given 'Chan', forever.
--
-- Listens for requests in JSON format on stdin.
receiver :: Chan Request -> IO ()
receiver requests = forever $ do
  l <- BS.hGetLine stdin
  case Aeson.eitherDecode (BSL.fromStrict l) of
    Left err -> hPutStrLn stderr err
    Right TerminateProcess ->
      -- intercept terminate request here because main thread may be stuck on something.
      exitWith ExitSuccess
    Right req -> writeChan requests req

-- | Reads 'Response' messages from the given 'Chan', serializes them to the given handle, and sends them to the other end, forever
--
-- Writes for responses in JSON format on stdout.
sender :: Handle -> Chan Response -> IO ()
sender hout replies = forever $ do
  r <- readChan replies
  BSL.hPutStrLn hout $
    Aeson.encode r
