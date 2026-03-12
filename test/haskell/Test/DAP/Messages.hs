{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Messages where
----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.KeyMap (toHashMapText)
import qualified Data.Foldable as F
import           Control.Monad.Reader
import           Control.Exception hiding (handle)
import qualified Control.Exception as E
import qualified Data.ByteString            as BS
import qualified Data.HashMap.Strict as H
import           Data.List (findIndex, sortOn)
import           Network.Run.TCP
import           Network.Socket             (socketToHandle)
import           System.IO
import           System.Exit
import           Data.IORef
import           System.FilePath ((</>))
import           System.Random (randomRIO)
import qualified System.Process as P
import qualified Data.Text as T
----------------------------------------------------------------------------
import           DAP.Utils
import           DAP.Server
----------------------------------------------------------------------------
import           Test.Tasty.HUnit

----------------------------------------------------------------------------
-- * Monad for DAP client context
----------------------------------------------------------------------------

data TestDAPClientContext = TestDAPClientContext
  { clientHandle :: Handle
  , clientNextSeqRef :: IORef Int
  }

newtype TestDAP a = TestDAP { runTestDAP :: TestDAPClientContext -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader TestDAPClientContext) via (ReaderT TestDAPClientContext IO)

--------------------------------------------------------------------------------
-- * Message primitives
--------------------------------------------------------------------------------

send :: [Pair] -> TestDAP ()
send message = do
  TestDAPClientContext{..} <- ask
  seqNum <- liftIO $ atomicModifyIORef' clientNextSeqRef (\n -> (n + 1, n))
  liftIO $
    BS.hPutStr clientHandle $
      encodeBaseProtocolMessage (object ("seq" .= seqNum : filter ((/= "seq") . fst) message))

data MessageMatch = MessageMatch
  { messageMatchDescription :: String
  , messageMatchMatches :: Value -> Bool
  }

subsetMatch :: [Pair] -> MessageMatch
subsetMatch expected =
  MessageMatch ("subset: " ++ show (object expected)) $ \actual ->
    case (object expected, actual) of
      (Object ex, Object actualObj) ->
        toHashMapText ex `H.isSubmapOf` toHashMapText actualObj
      _ -> False

responseMatch :: String -> MessageMatch
responseMatch cmd =
  MessageMatch ("response(" ++ cmd ++ ")") $ \v ->
    maybe False id $ parseMaybe (withObject "response" $ \o -> do
      typ <- o .: "type"
      responseCmd <- o .: "command"
      success <- o .: "success"
      pure ((typ :: String) == "response" && (responseCmd :: String) == cmd && (success :: Bool))
      ) v

eventMatch :: String -> MessageMatch
eventMatch eventNameExpected =
  MessageMatch ("event(" ++ eventNameExpected ++ ")") $ \v ->
    maybe False id $ parseMaybe (withObject "event" $ \o -> do
      typ <- o .: "type"
      eventName <- o .: "event"
      pure ((typ :: String) == "event" && (eventName :: String) == eventNameExpected)
      ) v

receiveMessagesUnordered :: [MessageMatch] -> TestDAP [Value]
receiveMessagesUnordered filters = go (zip [0 :: Int ..] filters) []
  where
    go [] matched = pure (map snd (sortOn fst matched))
    go remaining matched = do
      actual <- nextPayload
      case findIndex (\(_, f) -> messageMatchMatches f actual) remaining of
        Nothing -> do
          let pending = map (messageMatchDescription . snd) remaining
          liftIO $ assertFailure $
            "Unexpected message: " ++ show actual ++ "\nPending filters: " ++ show pending
          pure []
        Just idx ->
          case splitAt idx remaining of
            (before, (i, _) : after) ->
              go (before ++ after) ((i, actual) : matched)
            _ -> fail "Internal error in receiveMessagesUnordered"

    nextPayload :: TestDAP Value
    nextPayload = do
      TestDAPClientContext{clientHandle = h} <- ask
      payload <- liftIO $ readPayload h
      case payload of
        Left e -> fail e
        Right actual -> pure actual

expectMessagesUnordered :: [MessageMatch] -> TestDAP ()
expectMessagesUnordered filters = do
  _ <- receiveMessagesUnordered filters
  pure ()

--------------------------------------------------------------------------------
