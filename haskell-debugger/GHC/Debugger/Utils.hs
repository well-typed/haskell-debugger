{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Utils
  ( module GHC.Debugger.Utils
  , module GHC.Utils.Outputable
  , module GHC.Utils.Trace
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception

import GHC
import GHC.Data.FastString
import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Utils.Outputable hiding (char)
import GHC.Utils.Trace
import qualified Data.Text as T

import Data.Attoparsec.Text

import GHC.Debugger.Monad
import GHC.Debugger.Logger as Logger
import GHC.Debugger.Interface.Messages

--------------------------------------------------------------------------------
-- * GHC Utilities
--------------------------------------------------------------------------------

-- | Convert a GHC's src span into an interface one
realSrcSpanToSourceSpan :: RealSrcSpan -> SourceSpan
realSrcSpanToSourceSpan ss = SourceSpan
  { file = unpackFS $ srcSpanFile ss
  , startLine = srcSpanStartLine ss
  , startCol = srcSpanStartCol ss
  , endLine = srcSpanEndLine ss
  , endCol = srcSpanEndCol ss
  }

-- | Display an Outputable value as a String
display :: Outputable a => a -> Debugger String
display x = do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

--------------------------------------------------------------------------------
-- * More utils
--------------------------------------------------------------------------------

expectRight :: Exception e => Either e a -> Debugger a
expectRight s = case s of
  Left e -> do
    logSDoc Logger.Error (text $ displayException e)
    liftIO $ throwIO e
  Right a -> do
    pure a

--------------------------------------------------------------------------------
-- * Parsing
--------------------------------------------------------------------------------

-- | Takes a 'srcLoc' string from 'StackEntry' and returns a 'SourceSpan'.
--
-- === Example strings
--
-- - @hdb/Development/Debug/Adapter/Init.hs:(188,15)-(197,48)@
-- - @hdb/Development/Debug/Adapter/Proxy.hs:93:34-37@
srcSpanStringToSourceSpan :: String -> Either String SourceSpan
srcSpanStringToSourceSpan s = parseOnly pSrcSpan (T.pack s)
  where
    pSrcSpan = do
      fp <- pFile <* char ':'
      pParenStyle fp <|> pColonStyle fp

    -- file:(l1,c1)-(l2,c2)
    pParenStyle fp = do
      (l1, c1) <- (,) <$> (char '(' *> num <* char ',') <*> (num <* char ')') <* char '-'
      (l2, c2) <- (,) <$> (char '(' *> num <* char ',') <*> (num <* char ')')
      pure (SourceSpan fp l1 l2 c1 c2)

    -- file:l1:c1-c2
    pColonStyle fp = do
      l1 <- num <* char ':'
      c1 <- num <* char '-'
      c2 <- num
      pure (SourceSpan fp l1 l1 c1 c2)

    pFile :: Parser FilePath
    pFile = T.unpack <$> takeTill (== ':')

    num :: Parser Int
    num = decimal

