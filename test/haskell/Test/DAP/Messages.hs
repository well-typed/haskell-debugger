{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Messages where
----------------------------------------------------------------------------
import           Control.Concurrent.Async
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Control.Monad.Reader
import qualified Data.ByteString            as BS
import           System.IO
import           Data.IORef
----------------------------------------------------------------------------
import           DAP.Utils
import Control.Concurrent.STM
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- * Monad for DAP client context
----------------------------------------------------------------------------

data TestDAPClientContext = TestDAPClientContext
  { clientHandle :: Handle
    -- ^ Connection to server
  , clientNextSeqRef :: IORef Int
    -- ^ Counter for seq numbers
  , clientResponses :: TChan Value
    -- ^ Collect response messages sent by server
  , clientEvents :: TChan Value
    -- ^ Collect event messages sent by server
  , clientReverseRequests :: TChan Value
    -- ^ Collect reverse requests messages sent by server
  , clientSupportsRunInTerminal :: Bool
    -- ^ Run test with runInTerminal support?
  }

newtype TestDAP a = TestDAP { runTestDAP :: TestDAPClientContext -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader TestDAPClientContext) via (ReaderT TestDAPClientContext IO)

--------------------------------------------------------------------------------
-- * Message primitives
--------------------------------------------------------------------------------

type ResponseCont b a = Async b -> TestDAP a

-- | Run an action with an Async in the continuation synchronously by simply
-- waiting for the response.
sync :: (ResponseCont b b -> TestDAP b) -> TestDAP b
sync k = k (liftIO . wait)

-- | Send message with next sequence number and expect a response (response
-- value is given as async in continuation)
send :: [Pair] -> ResponseCont Value a -> TestDAP a
send message k = do
  ctx@TestDAPClientContext{..} <- ask
  seqNum <- liftIO $ atomicModifyIORef' clientNextSeqRef (\n -> (n + 1, n))
  liftIO $ do
    BS.hPutStr clientHandle $
      encodeBaseProtocolMessage (object ("seq" .= seqNum : filter ((/= "seq") . fst) message))

    withAsync (runTestDAP waitForResponse ctx) $ \v ->
      runTestDAP (k v) ctx

-- | Reply to reverse request of given seq number
reply :: Int -> [Pair] -> TestDAP ()
reply revReqSeqNum message = do
  TestDAPClientContext{..} <- ask
  liftIO $ do
    BS.hPutStr clientHandle $
      encodeBaseProtocolMessage (object ("seq" .= (revReqSeqNum + 1) : filter ((/= "seq") . fst) message))

waitForResponse :: TestDAP Value
waitForResponse = do
  TestDAPClientContext{..} <- ask
  liftIO $ atomically $ readTChan clientResponses

waitForReverseRequest :: TestDAP Value
waitForReverseRequest = do
  TestDAPClientContext{..} <- ask
  liftIO $ atomically $ readTChan clientReverseRequests

waitForEvent :: TestDAP Value
waitForEvent = do
  TestDAPClientContext{..} <- ask
  liftIO $ atomically $ readTChan clientEvents

