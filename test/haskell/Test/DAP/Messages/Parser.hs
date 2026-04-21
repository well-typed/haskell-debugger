{-# LANGUAGE OverloadedStrings #-}
module Test.DAP.Messages.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.KeyMap
import qualified Data.HashMap.Strict as H
import Test.DAP.Orphans ()

--------------------------------------------------------------------------------
-- * MessageMatch
--------------------------------------------------------------------------------

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

reverseRequestMatch :: String -> MessageMatch
reverseRequestMatch commandExpected =
  MessageMatch ("reverseRequest(" ++ commandExpected ++ ")") $ \v ->
    maybe False id $ parseMaybe (withObject "reverseRequest" $ \o -> do
      typ <- o .: "type"
      command <- o .: "command"
      pure ((typ :: String) == "request" && (command :: String) == commandExpected)
      ) v

--------------------------------------------------------------------------------
-- * Message parsers
--------------------------------------------------------------------------------

data Response a = Response
  { responseSuccess :: Bool
  , responseBody :: Maybe a }

data Event a = Event
  { eventEvent :: String
  , eventBody  :: Maybe a }

instance FromJSON a => FromJSON (Response a) where
  parseJSON = withObject "parsing from .body" $ \o -> do
    body <- o .:? "body"
    success <- o .: "success"
    pure $ Response success body

instance FromJSON a => FromJSON (Event a) where
  parseJSON = withObject "parsing from .body" $ \o -> do
    body <- o .:? "body"
    event <- o .: "event"
    pure $ Event event body
