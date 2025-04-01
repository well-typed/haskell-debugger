{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Posix.IO
import System.Posix.Types
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import GHC.IO.Handle

-- === Simulated JSON channel ===
-- In real use, this could be a socket, pipe, etc.
sendJsonMessage :: String -> IO ()
sendJsonMessage msg = do
  let json = "{\"type\": \"third_party_output\", \"message\": \"" ++ msg ++ "\"}"
  BS.hPutStrLn stderr (BS.pack json)

-- === Your code writes directly to the terminal ===
myCode :: Handle -> IO ()
myCode bypass = do
  replicateM_ 10 $ do
    threadDelay 10000
    BS.hPutStrLn bypass "My code: writing to real stdout!"

-- === Simulated third-party code (we can't change this) ===
thirdPartyCode :: IO ()
thirdPartyCode = do
  replicateM_ 10 $ do
    threadDelay 10000
    putStrLn "Third-party: line 1"
    putStrLn "Third-party: line 2"
    putStrLn "Third-party: done"

-- === Redirected stdout processing ===
jsonForwardingThread :: Handle -> IO ()
jsonForwardingThread fromPipe = forever $ do
  line <- BS.hGetLine fromPipe
  sendJsonMessage (BS.unpack line)

main :: IO ()
main = do
  -- Save original stdout (bypass handle for your code)
  realStdoutFd <- dup stdOutput
  realStdout <- fdToHandle realStdoutFd

  -- Create a pipe to capture stdout
  (readFd, writeFd) <- createPipe
  readHandle  <- fdToHandle readFd
  writeHandle <- fdToHandle writeFd

  -- Redirect global stdout to the pipe
  hFlush stdout
  hDuplicateTo writeHandle stdout
  hSetBuffering stdout LineBuffering

  -- Start thread to forward stdout lines as JSON
  _ <- forkIO $ jsonForwardingThread readHandle

  -- Launch your app and third-party code concurrently
  _ <- forkIO $ myCode realStdout
  thirdPartyCode

  -- Wait for a bit to let everything flush
  threadDelay 500000

  -- Clean up
  hFlush stdout
  hClose realStdout
  hClose writeHandle
  hClose readHandle

