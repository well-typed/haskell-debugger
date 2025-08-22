{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass, DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards #-}
module Development.Debug.Adapter.Handles
  ( handleLogger
  , withInterceptedStdout
  , withInterceptedStderr
  , withInterceptedStdoutForwarding
  , withInterceptedStderrForwarding
  ) where

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

-- | Redirect stdout globally, run the continuation with a bypass handle for real stdout.
withInterceptedStdout :: (Handle -- ^ realStdout, write to this handle sends output to stdout
                          -> Handle -- ^ interceptedStdout, stdout captured from attempting to write to stdout handle.
                          -> IO ()) -> IO ()
withInterceptedStdout k = do
  withPipe $ \readOutHandle writeOutHandle -> do
    hSetBuffering readOutHandle NoBuffering
    hSetBuffering writeOutHandle LineBuffering
    withStdoutBypass writeOutHandle $ \realStdout -> do
      k realStdout readOutHandle

-- | Redirect stderr globally, run the continuation with a bypass handle for real stderr.
withInterceptedStderr :: (Handle -- ^ realStderr, write to this handle sends output to stderr
                          -> Handle -- ^ interceptedStderr, stdout captured from attempting to write to stderr handle.
                          -> IO ()) -> IO ()
withInterceptedStderr k = do
  withPipe $ \readErrHandle writeErrHandle -> do
    hSetBuffering readErrHandle NoBuffering
    hSetBuffering writeErrHandle LineBuffering
    withStderrBypass writeErrHandle $ \realStderr -> do
      k realStderr readErrHandle

-- | Intercept stderr, and spawn a thread which forwards the input
-- onwards using the supplied IO action.
withInterceptedStderrForwarding :: (T.Text -> IO ())
                                -- ^ All stderr input that is intercepted is forwarded to this thread
                                -> (Handle -> IO ())
                                -- ^ The continuation receives the REAL STDERR
                                -> IO ()
withInterceptedStderrForwarding write_stderr k = do
  withInterceptedStderr $ \realStderr interceptedStderr -> do
      withAsync (forwardingThread write_stderr interceptedStderr) $ \_ -> do
        k realStderr

-- | Intercept stdout, and spawn a thread which forwards the input
-- onwards using the supplied IO action.
withInterceptedStdoutForwarding :: (T.Text -> IO ())
                                -- ^ All stdout input that is intercepted is forwarded to this thread
                                -> (Handle -> IO ())
                                -- ^ The continuation receives the REAL STDOUT
                                -> IO ()
withInterceptedStdoutForwarding write_stdout k = do
  withInterceptedStdout $ \realStdout interceptedStdout -> do
    withAsync (forwardingThread write_stdout interceptedStdout) $ \_ ->
        k realStdout

--------------------------------------------------------------------------------
-- Auxiliary
--------------------------------------------------------------------------------

-- | Temporarily bypass the intercepted stdout/stderr to write directly to the original stdout/stderr.
-- This is useful for debugging or for sending output that should not be intercepted.
withStdoutBypass, withStderrBypass
  :: Handle
  -> (Handle -> IO r)
  -> IO r
withStdoutBypass interceptH = withHandleBypass stdout interceptH
withStderrBypass interceptH = withHandleBypass stderr interceptH

-- | Capture all output written to a given handle and redirect it to the other
-- one; the continuation can use the "real" copy of the first handle to write
-- to it while bypassing the redirection.
--
-- This is useful for debugging or for sending output that should not be intercepted.
withHandleBypass :: Handle
                 -- ^ Text written to this handle...
                 -> Handle
                 -- ^ ...will be redirected to this handle
                 -> (Handle -> IO r)
                 -- ^ Continuation receives as an argument a "real" copy of the handle that is now being redirected.
                 -- If you write to this Handle, it will write to the original one and *bypass* the redirection (ie it will not be redirected)
                 -> IO r
withHandleBypass originalHandle interceptWriteHandle action =
  bracket setup clean action
  where
    setup = do
      realHandle <- hDuplicate originalHandle
      hFlush originalHandle
      hDuplicateTo interceptWriteHandle originalHandle
      hSetBuffering originalHandle LineBuffering
      return realHandle

    clean realHandle = do
      hFlush originalHandle
      hDuplicateTo realHandle originalHandle
      hClose realHandle

-- | Thread to read from the intercepted stdout pipe and forward onwards
forwardingThread :: (T.Text -> IO ()) -> Handle -> IO ()
forwardingThread write_action fromPipe = forever $ do
  line <- T.hGetLine fromPipe
  write_action line


withPipe :: (Handle -> Handle -> IO r) -> IO r
withPipe action = bracket createPipe closeBoth (uncurry action)
  where
    closeBoth (readH, writeH) = do
      hClose readH
      hClose writeH

