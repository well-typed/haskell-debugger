{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass, DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards #-}

-- TODO list:
--
-- [ ]
module Handles where

import DAP


import System.IO ()
import DAP.Log
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent.MVar
import GHC.IO.Handle.FD
import GHC.IO.Handle
import System.Process
import Control.Exception
import Control.Monad
import Data.IORef
import Control.Concurrent.Async

{- Note [Debugger Handles]
~~~~~~~~~~~~~~~~~~~~~~~~~~

The debugger executes arbritary user programs which will output to stdout.
This output needs to be sent to the debug client. Any output we wish to put in a
log or elsewhere needs to be directed to another handle.


-}

handleLogger :: Handle -> IO (LogAction IO T.Text)
handleLogger out_handle = do
  handleLock               <- newMVar ()
  return $ LogAction $ \msg -> do
    withLock handleLock $ do
      T.hPutStrLn out_handle msg

type ForwardingRef = IORef (T.Text -> IO ())

withPipe :: (Handle -> Handle -> IO r) -> IO r
withPipe action = bracket createPipe closeBoth (uncurry action)
  where
    closeBoth (readH, writeH) = do
      hClose readH
      hClose writeH


-- | Temporarily bypass the intercepted stdout to write directly to the original stdout.
-- This is useful for debugging or for sending output that should not be intercepted.
withStdoutBypass :: Handle -> (Handle -> IO r) -> IO r
withStdoutBypass writeHandle action =
  bracket setup clean action
  where
    setup = do
      realStdout <- hDuplicate stdout
      hFlush stdout
      hDuplicateTo writeHandle stdout
      hSetBuffering stdout LineBuffering
      return realStdout

    clean realStdout = do
      hFlush stdout
      hDuplicateTo realStdout stdout
      hClose realStdout


-- | Redirect stdout globally, run the continuation with a bypass handle for real stdout.
withInterceptedStdout :: (Handle -- ^ realStdout, write to this handle sends output to stdout
                          -> Handle -- ^ interceptedStdout, stdout captured from attempting to write to stdout handle.
                          -> IO ()) -> IO ()
withInterceptedStdout k = do
  withPipe $ \readHandle writeHandle -> do
    hSetBuffering readHandle NoBuffering
    hSetBuffering writeHandle LineBuffering
    withStdoutBypass writeHandle $ \realStdout -> do
      k realStdout readHandle


-- | Intercept stdout, and spawn a thread which forwards the input onwards using the
-- supplied IO action.
withInterceptedStdoutForwarding :: (T.Text -> IO ()) -> (Handle -> IO ()) -> IO ()
withInterceptedStdoutForwarding write_stdout k = do
  withInterceptedStdout $ \realStdout interceptedStdout -> do
    withAsync (forwardingThread write_stdout interceptedStdout) $ \_ -> do
      k realStdout


-- | Thread to read from the intercepted stdout pipe and forward onwards
forwardingThread :: (T.Text -> IO ()) -> Handle -> IO ()
forwardingThread write_stdout fromPipe = forever $ do
  line <- T.hGetLine fromPipe
  write_stdout line



