{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Parser where

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

parseThreadId :: Value -> Maybe Int
parseThreadId = parseMaybe $ withObject "threads response" $ \o -> do
  body <- o .: "body"
  withObject "threads body" (\b -> do
    threads <- b .: "threads"
    withArray "threads" (\arr -> do
      case F.toList arr of
        [] -> fail "no threads"
        x : _ -> withObject "thread" (\t -> t .: "id") x
      ) threads
    ) body

parseFrameId :: Value -> Maybe Int
parseFrameId = parseMaybe $ withObject "stackTrace response" $ \o -> do
  body <- o .: "body"
  withObject "stackTrace body" (\b -> do
    frames <- b .: "stackFrames"
    withArray "stackFrames" (\arr -> do
      case F.toList arr of
        [] -> fail "no stack frames"
        x : _ -> withObject "frame" (\f -> f .: "id") x
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

