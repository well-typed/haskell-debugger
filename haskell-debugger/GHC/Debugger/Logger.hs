{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- | Simple Logger API using co-log style loggers
module GHC.Debugger.Logger (
  -- * The core Logger type
  Recorder,
  logWith,
  -- * Log messages
  Pretty(..),
  -- * For simpler usage
  Colog.LogAction (..),
  toCologAction,
  fromCologAction,
  -- * Severity
  Severity (..),
  WithSeverity (..),
  cmap,
  cmapIO,
  cmapWithSev,

  -- * Pretty printing of logs
  renderPrettyWithSeverity,
  renderWithSeverity,
  renderPretty,
  renderSeverity,
  renderWithTimestamp,

  -- Re-exports
  module Data.Functor.Contravariant,
) where

import GHC.Stack

import Control.Monad.IO.Class
import Control.Monad ((>=>))

import Colog.Core (Severity(..), WithSeverity(..))
import qualified Colog.Core as Colog
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

newtype Recorder msg = Recorder
  { logger_ :: forall m. (MonadIO m) => msg -> m () }

instance Contravariant Recorder where
  contramap f Recorder{ logger_ } =
    Recorder
      { logger_ = logger_ . f }

instance Semigroup (Recorder msg) where
  (<>) Recorder{ logger_ = logger_1 } Recorder{ logger_ = logger_2 } =
    Recorder
      { logger_ = \msg -> logger_1 msg >> logger_2 msg }

instance Monoid (Recorder msg) where
  mempty =
    Recorder
      { logger_ = \_ -> pure () }

logWith :: (HasCallStack, MonadIO m) => Recorder (WithSeverity msg) -> Severity -> msg -> m ()
logWith (Recorder logger_) sev msg = logger_ $ WithSeverity msg sev

cmap :: (a -> b) -> Recorder b -> Recorder a
cmap = contramap

cmapWithSev :: (a -> b) -> Recorder (WithSeverity b) -> Recorder (WithSeverity a)
cmapWithSev f = contramap (fmap f)

cmapIO :: (a -> IO b) -> Recorder b -> Recorder a
cmapIO f Recorder{ logger_ } =
  Recorder
    { logger_ = (liftIO . f) >=> logger_ }

renderPrettyWithSeverity :: Pretty a => WithSeverity a -> Text
renderPrettyWithSeverity =
  renderWithSeverity renderPretty

renderWithSeverity :: (a -> Text) -> WithSeverity a -> Text
renderWithSeverity f msgWithSev =
  renderSeverity (getSeverity msgWithSev) <> " " <> f (getMsg msgWithSev)

renderPretty :: Pretty a => a -> Text
renderPretty a =
  let
    docToText = renderStrict . layoutPretty defaultLayoutOptions
  in
    docToText (pretty a)

renderWithTimestamp :: Text -> IO Text
renderWithTimestamp msg = do
  t <- getCurrentTime
  let timeStamp = utcTimeToText t
  pure $ "[" <> timeStamp <> "]" <> msg
  where
    utcTimeToText utcTime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" utcTime

renderSeverity :: Severity -> Text
renderSeverity = \ case
  Debug -> "[DEBUG]"
  Info -> "[INFO]"
  Warning -> "[WARNING]"
  Error -> "[ERROR]"

toCologAction :: (MonadIO m, HasCallStack) => Recorder msg -> Colog.LogAction m msg
toCologAction (Recorder logger_) = Colog.LogAction $ \msg -> do
    logger_ msg

fromCologAction :: (HasCallStack) => Colog.LogAction IO msg -> Recorder msg
fromCologAction (Colog.LogAction logger_) = Recorder $ \msg -> do
    liftIO $ logger_ msg
