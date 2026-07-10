{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Utils
  ( module GHC.Debugger.Utils
  , module GHC.Utils.Outputable
  , module GHC.Utils.Trace
  , showSDoc
  ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Exception
import GHC.IO (unsafePerformIO)
import System.IO
import qualified System.Directory as D

import GHC
import GHC.Data.FastString
import GHC.Driver.DynFlags
import GHC.Driver.Ppr
import GHC.Utils.Outputable hiding (char)
import GHC.Utils.Trace
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text

import Colog.Core as Logger
import GHC.Debugger.Interface.Messages

--------------------------------------------------------------------------------
-- * Handle utils
--------------------------------------------------------------------------------

-- | Read output from the given handle and write it to the given
-- log action (forever).
forwardHandleToLogger :: Handle -> LogAction IO T.Text -> IO ()
forwardHandleToLogger read_h logger = do
  forwarding `catch` -- handles read EOF
    \(_e::SomeException) -> do
      -- Cleanly exit on exception
      -- print _e
      return ()
  where
    forwarding = forever $ do
      -- Mask exceptions to avoid being killed between reading
      -- a line and outputting it.
      mask_ $ do
        out_line <- T.hGetLine read_h -- See Note [External interpreter buffering]
        logger <& out_line

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
display :: (GhcMonad m, Outputable a) => a -> m String
display x = do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

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

--------------------------------------------------------------------------------
-- * DebugView utils
--------------------------------------------------------------------------------

showModule :: Module -> String
showModule = showSDocUnsafe . withPprStyle (PprDump alwaysQualify) . ppr

----------------------------------------------------
-- Current Directory utils
----------------------------------------------------
{-
Note [ Current directory is a global property that affects HIE and GHC ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have to change the current directory for hie, and we concurrent debugging sessions we have to make sure we play nice with other hie api calls.

Thanfully the hie calls should not be too long (as long as dependencies of the project are pre-built), so a solution is to make it a critical section with a global lock. The paths coming from HIE are made absolute at that point.

We also rely on the CWD as the projectRoot if none is given, or to interpret relative paths from the session or from the DAP requests. Ideally they would all be absolute paths but we don't have full control.
-}

{-# NOINLINE cwdLock #-}
cwdLock :: MVar ()
cwdLock = unsafePerformIO $ newMVar ()

-- | See Note [ Current directory is a global property that affects HIE and GHC ]
withCurrentDirectory :: FilePath -> IO b -> IO b
withCurrentDirectory fp m = withMVar cwdLock $ \ _ -> D.withCurrentDirectory fp m

-- | See Note [ Current directory is a global property that affects HIE and GHC ]
withOriginalCurrentDirectory :: (FilePath -> IO b) -> IO b
withOriginalCurrentDirectory m = withMVar cwdLock $ \ _ ->
  m =<< D.getCurrentDirectory
