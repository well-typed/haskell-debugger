-- | Utils essentially copied from `dap`'s test suite
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Test.DAP where

----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import           Data.Aeson.KeyMap
import           Control.Exception hiding (handle)
import qualified Data.ByteString.Lazy.Char8 as BL8 ( hPutStrLn )
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B8 ( hPutStrLn )
import qualified Data.HashMap.Strict as H
import           Network.Run.TCP
import           Network.Socket             (socketToHandle)
import           System.IO
import           System.Exit
import           Data.IORef
----------------------------------------------------------------------------
import           DAP.Utils
import           DAP.Server
import           DAP.Types (Command)
----------------------------------------------------------------------------
import           Test.Tasty.HUnit

-- | Sends a DAP request to the given handle
sendDAPRequest :: ToJSON a => Handle -> Command -> a -> IO ()
sendDAPRequest handle cmd args = do
  send handle
    [ "seq"      .= (1 :: Int)
    , "type"     .= ("request" :: String)
    , "command"  .= cmd
    , "arguments".= args
    ]

-- | Receive and decode DAP message
recvDAPResponse
  :: FromJSON a
  => Handle
  -- ^ Handle to receive bytes from
  -> IO a
  -- ^ The decoded DAP message
recvDAPResponse h = do
  readPayload h >>= \case
    Left e -> fail e
    Right actual ->
      -- Read field "body" or "arguments" (for reverse requests)
      case parseMaybe (withObject "response/rev request" $ \r -> (do
              ("response" :: String) <- r .: "type"
              True                   <- r .: "success"
              r .: "body") <|> (do
                ("request" :: String) <- r .: "type"
                r .: "arguments")
            ) actual of
        Nothing -> fail $ "Failed to parse DAP response body: " ++ show actual
        Just body ->
          case parseMaybe parseJSON body of
            Nothing -> fail $ "Failed to parse DAP response body content: " ++ show body
            Just res -> pure res

--------------------------------------------------------------------------------

-- | Sample host shared amongst client and server
--
testHost :: String
testHost = "127.0.0.1"

-- | Spawns a new mock client that connects to the mock server.
--
withNewClient :: Int -- ^ Port
              -> IORef Bool
              -- ^ True if we've already connected once and therefore should no longer retry
              -> (Handle -> IO ())
              -> IO ()
withNewClient port retryVar continue = flip catch exceptionHandler $
  runTCPClient testHost (show port) $ \socket -> do
    h <- socketToHandle socket ReadWriteMode
    hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
    continue h `finally` hClose h
      where
        exceptionHandler :: SomeException -> IO ()
        exceptionHandler e = do
          do_retry <- readIORef retryVar
          if do_retry then do
            threadDelay 100_000 -- 0.1s
            -- Do it silently:
            -- putStrLn "Retrying connection..."
            withNewClient port retryVar continue
          else do
            putStrLn $ displayException e
            exitWith (ExitFailure 22)

-- | Helper to send JSON payloads to the server
--
send :: Handle -> [Pair] -> IO ()
send h message
  = BS.hPutStr h $ encodeBaseProtocolMessage (object message)

-- | Helper to receive JSON payloads to the client
-- checks if 'Handle' returns a subset expected payload
--
shouldReceive
  :: Handle
  -- ^ Handle to receive bytes from
  -> [Pair]
  -- ^ Subset of JSON values that should be present in the payload
  -> IO ()
shouldReceive h expected = do
  case object expected of
    Object ex ->
      readPayload h >>= \case
        Left e -> fail e
        Right actual
          | toHashMapText ex `H.isSubmapOf` toHashMapText actual -> pure ()
          | otherwise -> encodePretty actual @=? encodePretty ex
    _ -> fail "Invalid JSON"
