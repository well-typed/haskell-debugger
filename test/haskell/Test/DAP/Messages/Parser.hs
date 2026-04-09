{-# LANGUAGE OverloadedStrings #-}
module Test.DAP.Messages.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.KeyMap
import qualified Data.HashMap.Strict as H
import qualified Data.Foldable as F
import qualified Data.Text as T

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
-- TODO: ALL OF THESE SHOULD BE AUTOMATICALLY FROMJSON FOR RESPONSE TYPES!!!!

parseThreadIds :: Value -> Maybe [Int]
parseThreadIds = parseMaybe $ withObject "threads response" $ \o -> do
  body <- o .: "body"
  withObject "threads body" (\b -> do
    threads <- b .: "threads"
    withArray "threads" (\arr -> do
      mapM (withObject "thread" (\t -> t .: "id")) $ F.toList arr
      ) threads
    ) body

parseFramesIds :: Value -> Maybe [Int]
parseFramesIds = parseMaybe $ withObject "stackTrace response" $ \o -> do
  body <- o .: "body"
  withObject "stackTrace body" (\b -> do
    frames <- b .: "stackFrames"
    withArray "stackFrames" (\arr -> do
      mapM (withObject "frame" (\f -> f .: "id")) $ F.toList arr
      ) frames
    ) body

parseScopes :: Value -> Maybe [(String, Bool)]
parseScopes = parseMaybe $ withObject "scopes response" $ \o -> do
  body <- o .: "body"
  withObject "scopes body" (\b -> do
    scopes <- b .: "scopes"
    withArray "scopes" (\arr ->
      mapM
        (withObject "scope" (\s -> do
          name <- s .: "name"
          expensive <- s .: "expensive"
          pure (name, expensive)
          ))
        (F.toList arr)
      ) scopes
    ) body

parseStoppedEventReason :: Value -> Maybe String
parseStoppedEventReason = parseMaybe $ withObject "stopped event" $ \o -> do
    body <- o .: "body"
    String reason <- body .: "reason"
    pure $ T.unpack reason

parseOutput :: Value -> Maybe T.Text
parseOutput = parseMaybe $ withObject "event" $ \ o -> do
    body <- o .: "body"
    String output <- body .: "output"
    pure output

