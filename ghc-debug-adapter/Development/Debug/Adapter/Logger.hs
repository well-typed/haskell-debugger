{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple Logger API using co-log style loggers
module Development.Debug.Adapter.Logger (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
  cmap,
  cmapWithSev,

  -- * Pretty printing of logs
  renderPrettyWithSeverity,
  renderWithSeverity,
  renderPretty,
  renderSeverity,
) where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Colog.Core.Action (cmap)
import Data.Text (Text)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

cmapWithSev :: (a -> b) -> LogAction m (WithSeverity b) -> LogAction m (WithSeverity a)
cmapWithSev f = cmap (fmap f)

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

renderSeverity :: Severity -> Text
renderSeverity = \ case
  Debug -> "[DEBUG]"
  Info -> "[INFO]"
  Warning -> "[WARNING]"
  Error -> "[ERROR]"
