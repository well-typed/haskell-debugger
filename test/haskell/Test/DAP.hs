-- | Utils essentially copied from `dap`'s test suite
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Test.DAP where

----------------------------------------------------------------------------
import           Control.Concurrent
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import           Data.Aeson.KeyMap
import           Control.Exception hiding (handle)
import qualified Data.ByteString.Lazy.Char8 as BL8 ( hPutStrLn )
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.HashMap.Strict as H
import           Network.Run.TCP
import           Network.Socket             (socketToHandle)
import           System.IO
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
      -- Read field "body"
      case parseMaybe (withObject "response" $ \r -> do
              ("response" :: String) <- r .: "type"
              True                   <- r .: "success"
              r .: "body"
            ) actual of
        Nothing -> fail $ "Failed to parse DAP response body: " ++ show actual
        Just body ->
          case parseMaybe parseJSON body of
            Nothing -> fail $ "Failed to parse DAP response body content: " ++ show body
            Just res -> pure res

--------------------------------------------------------------------------------

-- | Sample port shared amongst client and server
--
testPort :: Int
testPort = 8033

-- | Sample host shared amongst client and server
--
testHost :: String
testHost = "127.0.0.1"

-- | Spawns a new mock client that connects to the mock server.
--
withNewClient :: (Handle -> IO ()) -> IO ()
withNewClient continue = flip catch exceptionHandler $
  runTCPClient testHost (show testPort) $ \socket -> do
    h <- socketToHandle socket ReadWriteMode
    hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
    continue h `finally` hClose h
      where
        exceptionHandler :: SomeException -> IO ()
        exceptionHandler _ = do
          threadDelay 100
          putStrLn "Retrying connection..."
          withNewClient continue

-- | Helper to send JSON payloads to the server
--
send :: Handle -> [Pair] -> IO ()
send h message
  = BL8.hPutStrLn h $ LBS.fromStrict $ encodeBaseProtocolMessage (object message)

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
