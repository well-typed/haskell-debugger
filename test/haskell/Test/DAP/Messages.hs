{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Messages where
----------------------------------------------------------------------------
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Types
import           Control.Monad.Reader
import qualified Data.ByteString            as BS
import           System.IO
import           Data.IORef
import           GHC.Stack
----------------------------------------------------------------------------
import           DAP.Utils
import qualified Data.Text as T
import Test.DAP.Messages.Parser
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
  , clientConnectionClosed :: TVar Bool
    -- ^ Is the connection with the server closed? If so, no more messages will be added to the TChans.
  , clientFullOutput :: TVar [T.Text]
    -- ^ The full output is available here in reverse order (from most recent to oldest output strings).
    --
    -- The output events are STILL available from the events channel (this
    -- might be useful if you want to check a certain output event happens
    -- after some other specific event like a stopped one, rather than just
    -- overall).
    --
    -- We keep this full text because it is often useful to query the full
    -- output and not care about ordering.
  , clientSupportsRunInTerminal :: Bool
    -- ^ Run test with runInTerminal support?
  , clientHandleNoSuccess :: String -> Value -> IO (Maybe Value)
    -- ^ How to handle a response with success: false? If this function returns
    -- @Just val@ something then execution will resume with the returned @val@
    -- rather than aborting.
  , clientCurrentActiveThread :: IORef Int
    -- ^ When a breakpoint is hit, it reports the thread which hit the breakpoint
    -- and we store that and consider it to be the the currently active thread.
    --
    -- In a multi-threaded scenario where many threads are resumed simultaneously,
    -- the current active thread may vary a lot (receiving many StoppedEvents) but
    -- at that point why are you using commands which rely on the current active
    -- thread (the global buttons)?
    --
    -- (Note: the StoppedEvents to update this are caught directly in the
    -- message handler thread)
  }

newtype TestDAP a = TestDAP { runTestDAP :: TestDAPClientContext -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader TestDAPClientContext, MonadThrow, MonadCatch, MonadMask) via (ReaderT TestDAPClientContext IO)

--------------------------------------------------------------------------------
-- * Message primitives
--------------------------------------------------------------------------------

type AsyncCont b a = Async b -> TestDAP a
type ResponseCont b a = Async (Response b) -> TestDAP a

-- | Run an action with an Async in the continuation synchronously by simply
-- waiting for the response.
sync :: (ResponseCont b (Response b) -> TestDAP (Response b)) -> TestDAP (Response b)
sync k = k (liftIO . wait)

-- | Send message with next sequence number and expect a response (response
-- value is given as async in continuation)
send :: forall b r. FromJSON b => [Pair] -> ResponseCont b r -> TestDAP r
send message k = do
  ctx@TestDAPClientContext{..} <- ask
  seqNum <- liftIO $ atomicModifyIORef' clientNextSeqRef (\n -> (n + 1, n))
  liftIO $ do
    BS.hPutStr clientHandle $
      encodeBaseProtocolMessage (object ("seq" .= seqNum : filter ((/= "seq") . fst) message))

    withAsync (runTestDAP waitForResponse ctx) $ \v ->
      runTestDAP (k $ (\r -> unwrap (fromJSON @(Response b) r) r) <$> v) ctx
        where
          unwrap (Error e) r = error ("send: Parsing 'Response' failed with " ++ show e ++ " for message: " ++ show r)
          unwrap (Success x) _ = x

-- | Reply to reverse request of given seq number
reply :: Int -> [Pair] -> TestDAP ()
reply revReqSeqNum message = do
  TestDAPClientContext{..} <- ask
  liftIO $ do
    BS.hPutStr clientHandle $
      encodeBaseProtocolMessage (object ("seq" .= (revReqSeqNum + 1) : filter ((/= "seq") . fst) message))

data TestDAPClientConnectionClosed = TestDAPClientConnectionClosed
  deriving Show
instance Exception TestDAPClientConnectionClosed

data MsgType = EventTy | ResponseTy | ReverseRequestTy
  deriving Show

msgChan :: MsgType -> TestDAPClientContext -> TChan Value
msgChan ty TestDAPClientContext{..} = case ty of
  EventTy          -> clientEvents
  ResponseTy       -> clientResponses
  ReverseRequestTy -> clientReverseRequests

readMessage :: HasCallStack => TestDAPClientContext -> MsgType -> STM Value
readMessage ctx@TestDAPClientContext{..} ty = do
  let c = msgChan ty ctx
  m <- tryReadTChan c
  case m of
    Nothing -> do
      readTVar clientConnectionClosed >>= \case
        True -> throwSTM TestDAPClientConnectionClosed
        False -> retry
    Just msg -> pure msg

waitForResponse :: HasCallStack => TestDAP Value
waitForResponse = do
  ctx <- ask
  liftIO $ atomically $ readMessage ctx ResponseTy

waitForReverseRequest :: HasCallStack => TestDAP Value
waitForReverseRequest = do
  ctx <- ask
  liftIO $ atomically $ readMessage ctx ReverseRequestTy

waitForEvent :: HasCallStack => TestDAP Value
waitForEvent = do
  ctx <- ask
  liftIO $ atomically $ readMessage ctx EventTy

-- | See 'clientCurrentActiveThread'
getCurrentActiveThread :: TestDAP Int
getCurrentActiveThread = do
  liftIO . readIORef =<< asks clientCurrentActiveThread
