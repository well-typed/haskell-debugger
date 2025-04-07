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

The debugger executes arbritary user programs which will output to stdout and stderr.
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

-- | Temporarily bypass the intercepted stdout/stderr to write directly to the original stdout/stderr.
-- This is useful for debugging or for sending output that should not be intercepted.
withStdoutBypass :: Handle
                 -- ^ Text written to @'stdout'@ will be redirected to this handle
                 -> Handle
                 -- ^ Text written to @'stderr'@ will be redirected to this handle
                 -> ((Handle {-^ real stdout -}, Handle {-^ real stderr -}) -> IO r)
                 -- ^ Continuation receives as an argument the REAL STDOUT and REAL STDERR
                 -> IO r
withStdoutBypass writeOutHandle writeErrHandle action =
  bracket setup clean action
  where
    setup = (,) <$> setup' writeOutHandle stdout
                <*> setup' writeErrHandle stderr

    setup' redirect_handle origin_handle = do
      realHandle <- hDuplicate origin_handle
      hFlush origin_handle
      hDuplicateTo redirect_handle origin_handle
      hSetBuffering origin_handle LineBuffering
      return realHandle

    clean (realStdout, realStderr) = do
      hFlush stdout
      hFlush stderr
      hDuplicateTo realStdout stdout
      hDuplicateTo realStderr stderr
      hClose realStdout
      hClose realStderr


-- | Redirect stdout globally, run the continuation with a bypass handle for real stdout.
withInterceptedStdout :: ((Handle{-^ real stdout -}, Handle{-^ real stderr -}) -- ^ realStdout and realStderr, write to this handle sends output to stdout/stderr
                          -> (Handle{-^ intercepted stdout -}, Handle{-^ real stderr -}) -- ^ interceptedStdout and interceptedStderr, stdout/stderr captured from attempting to write to stdout/stderr handle.
                          -> IO ()) -> IO ()
withInterceptedStdout k = do
  withPipe $ \readOutHandle writeOutHandle -> do
    withPipe $ \readErrHandle writeErrHandle -> do
      mapM_ (`hSetBuffering` NoBuffering)   [readOutHandle, readErrHandle]
      mapM_ (`hSetBuffering` LineBuffering) [writeOutHandle, writeErrHandle]
      withStdoutBypass writeOutHandle writeErrHandle $ \(realStdout, realStderr) -> do
        k (realStdout, realStderr) (readOutHandle, readErrHandle)


-- | Intercept stdout and stderr, and spawn a thread which forwards the input
-- onwards using the supplied IO action.
withInterceptedStdoutForwarding :: (T.Text -> IO ())
                                -- ^ All stdout input that is intercepted is forwarded to this thread
                                -> (T.Text -> IO ())
                                -- ^ All stderr input that is intercepted is forwarded to this thread(Handle -> IO r)
                                -> ((Handle{-^ real stdout -}, Handle{-^ real stderr -}) -> IO ())
                                -- ^ The continuation receives the REAL STDOUT and REAL STDERR
                                -> IO ()
withInterceptedStdoutForwarding write_stdout write_stderr k = do
  withInterceptedStdout $ \(realStdout, realStderr) (interceptedStdout, interceptedStderr) -> do
    withAsync (forwardingThread write_stdout interceptedStdout) $ \_ ->
      withAsync (forwardingThread write_stderr interceptedStderr) $ \_ -> do
        k (realStdout, realStderr)


-- | Thread to read from the intercepted stdout pipe and forward onwards
forwardingThread :: (T.Text -> IO ()) -> Handle -> IO ()
forwardingThread write_action fromPipe = forever $ do
  line <- T.hGetLine fromPipe
  write_action line



