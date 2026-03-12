{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Init where

----------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception hiding (handle)
import qualified Control.Exception as E
import           Network.Run.TCP
import           Network.Socket             (socketToHandle)
import           System.IO
import           System.Exit
import           Data.IORef
import           System.Random (randomRIO)
import qualified System.Process as P
----------------------------------------------------------------------------
import           Test.DAP.Messages

data TestDAPServer = TestDAPServer
  { testDAPServerPort :: Int
  , testDAPServerProcess :: P.ProcessHandle
  , testDAPServerFlushOutput :: IO ()
  }

-- | Launch an @hdb server@ for tests on a random local port and capture stdout.
startTestDAPServer :: FilePath -> String -> IO TestDAPServer
startTestDAPServer testDir flags = do
  testPort <- randomRIO (49152, 65534) :: IO Int

  (Just _hin, Just hout, _, p)
    <- P.createProcess (P.shell $ "hdb server " ++ flags ++ " --port " ++ show testPort)
        { P.cwd = Just testDir
        , P.std_out = P.CreatePipe
        , P.std_in = P.CreatePipe
        }

  serverOutputRef <- newIORef []

  -- Fork thread to read output of server process. If we don't, the server
  -- blocks trying to write to stdout/stderr?
  _ <- forkIO $ flip catch (\(e :: IOException) -> print ("server process forwarding" :: String, e)) $ do
    hSetBuffering hout LineBuffering
    let loop = do
          eof <- hIsEOF hout
          if eof
            then return ()
            else do
              l <- hGetLine hout
              modifyIORef' serverOutputRef (l :)
              loop
    loop

  let flushServerOutput = do
        putStrLn "\n--- SERVER OUTPUT ---"
        readIORef serverOutputRef >>= mapM_ putStrLn . reverse
        putStrLn "---------------------\n"

  pure TestDAPServer
    { testDAPServerPort = testPort
    , testDAPServerProcess = p
    , testDAPServerFlushOutput = flushServerOutput
    }

-- | Connect a test client to a running 'TestDAPServer', with retry semantics
-- and server log flushing on failure.
withTestDAPServerClient :: TestDAPServer -> TestDAP a -> IO a
withTestDAPServerClient server continue = do
  retryVar <- newIORef True
  let runClient =
        withNewClient (testDAPServerPort server) retryVar $ \h -> do
          -- As soon as we get a connection, stop retrying
          writeIORef retryVar False
          seqRef <- newIORef 1
          runTestDAP continue (TestDAPClientContext h seqRef)
  (runClient `E.onException` testDAPServerFlushOutput server)
    `E.finally` P.terminateProcess (testDAPServerProcess server)

-- | Spawns a new mock client that connects to the mock server.
--
withNewClient :: forall a. Int -- ^ Port
              -> IORef Bool
              -- ^ True if we've already connected once and therefore should no longer retry
              -> (Handle -> IO a)
              -> IO a
withNewClient port retryVar continue = flip catch exceptionHandler $
  runTCPClient "127.0.0.1" (show port) $ \socket -> do
    h <- socketToHandle socket ReadWriteMode
    hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
    continue h `finally` hClose h
      where
        exceptionHandler :: SomeException -> IO a
        exceptionHandler e = do
          do_retry <- readIORef retryVar
          if do_retry then do
            threadDelay 100_000 -- 0.1s
            -- Do it silently:
            -- putStrLn "Retrying connection..."
            withNewClient port retryVar continue
          else do
            putStrLn $ displayException e
            exitWith (ExitFailure 22)
