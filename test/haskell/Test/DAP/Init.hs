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
import           Network.Socket             (Family(AF_INET), SockAddr(SockAddrInet, SockAddrInet6), SocketOption(ReuseAddr), SocketType(Stream), bind, close, defaultProtocol, getSocketName, setSocketOption, socket, socketToHandle, tupleToHostAddress)
import           System.IO
import           Control.Retry
import           Data.IORef
import qualified System.Process as P
----------------------------------------------------------------------------
import           Test.DAP.Messages
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad
import Data.Aeson.Types
import Test.Tasty.HUnit (assertFailure)
import DAP.Server (readPayload)
import qualified Control.Monad.Catch

--------------------------------------------------------------------------------
-- * Launch the DAP server process (what we're testing)
--------------------------------------------------------------------------------

data TestDAPServer = TestDAPServer
  { testDAPServerPort :: Int
  , testDAPServerProcess :: P.ProcessHandle
  , testDAPServerFlushOutput :: IO ()
  }

-- | Launch an @hdb server@ for tests on a random local port and capture stdout.
startTestDAPServer :: FilePath -> [String] -> IO TestDAPServer
startTestDAPServer testDir flags = do
  testPort <- getAvailablePort

  (Just _hin, Just hout, Just herr, p)
    <- P.createProcess (P.proc "hdb" $ ["server"] ++ flags ++ ["--port", show testPort])
        { P.cwd = Just testDir
        , P.std_out = P.CreatePipe
        , P.std_err = P.CreatePipe
        , P.std_in = P.CreatePipe
        }

  serverOutputRef <- newIORef []

  -- Spawn threads to read output of server process. If we don't, the server
  -- blocks trying to write to stdout/stderr?
  let forwardServerHandle h = flip catch (\(e :: IOException) -> print ("server process forwarding" :: String, e)) $ do
        hSetBuffering h LineBuffering
        let loop = do
              eof <- hIsEOF h
              if eof
                then return ()
                else do
                  l <- hGetLine h
                  modifyIORef' serverOutputRef (l :)
                  loop
        loop

  _ <- forkIO $ concurrently_ (forwardServerHandle hout) (forwardServerHandle herr)

  let flushServerOutput = do
        putStrLn "\n--- SERVER OUTPUT ---"
        readIORef serverOutputRef >>= mapM_ putStrLn . reverse
        putStrLn "---------------------\n"

  pure TestDAPServer
    { testDAPServerPort = testPort
    , testDAPServerProcess = p
    , testDAPServerFlushOutput = flushServerOutput
    }

getAvailablePort :: IO Int
getAvailablePort =
  bracket open close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 0 (tupleToHostAddress (0, 0, 0, 0)))
    getSocketName sock >>= \case
      SockAddrInet port _ -> pure (fromIntegral port)
      SockAddrInet6 port _ _ _ -> pure (fromIntegral port)
      addr -> error $ "getAvailablePort: unexpected socket address " ++ show addr
  where
    open = socket AF_INET Stream defaultProtocol

--------------------------------------------------------------------------------
-- * Launch the client connecting to the server (the test driver)
--------------------------------------------------------------------------------

-- | Like @withTestDAPServerClient'@ but also shutsdown server on exit.
withTestDAPServerClient :: Bool -> TestDAPServer -> TestDAP a -> IO a
withTestDAPServerClient clientSupportsRunInTerminal server continue = do
  withTestDAPServerClient' clientSupportsRunInTerminal server continue
    `E.finally` P.terminateProcess (testDAPServerProcess server)

--- | Connect a test client to a running 'TestDAPServer', with retry semantics
--- and server log flushing on failure.
withTestDAPServerClient' :: Bool {-^ Announce support for runInTerminal? -} -> TestDAPServer -> TestDAP a -> IO a
withTestDAPServerClient' clientSupportsRunInTerminal server continue = do
  runClient `E.onException` testDAPServerFlushOutput server
  where
    runClient = do
      withNewClient (testDAPServerPort server) $ \clientHandle -> do
        clientNextSeqRef      <- newIORef 1
        clientReverseRequests <- newTChanIO
        clientResponses       <- newTChanIO
        clientEvents          <- newTChanIO
        let ctx = TestDAPClientContext{..}
        either id (\() -> error "handleServerTestDAP unexpectedly returned") <$> race
          (runTestDAP continue ctx)
          (runTestDAP handleServerTestDAP ctx)

-- | Spawns a new mock client that connects to the mock server.
withNewClient :: forall a. Int -- ^ Port
              -> (Handle -> IO a)
              -> IO a
withNewClient port continue = do
  recovering (constantDelay 50000 <> limitRetries 50) retry_handlers $ \_ ->
    runTCPClient "127.0.0.1" (show port) $ \sock -> do
      h <- socketToHandle sock ReadWriteMode
      hSetNewlineMode h NewlineMode { inputNL = CRLF, outputNL = CRLF }
      continue h `finally` hClose h
  where
    retry_handlers =
      skipAsyncExceptions ++
      [const $ Control.Monad.Catch.Handler $ \ (_ :: SomeException) -> return True]

--------------------------------------------------------------------------------
-- ** Handle server responses, events, and reverse requests
--------------------------------------------------------------------------------

-- | Forever: read messages from handle and write them either to clientNonEvents or clientEvents
handleServerTestDAP :: TestDAP ()
handleServerTestDAP = do
  TestDAPClientContext{..} <- ask
  forever $ do
    payload <- nextPayload
    liftIO $ case parseMaybe parseType payload of
      Just "event"    -> atomically $ writeTChan clientEvents payload
      Just "response" -> atomically $ writeTChan clientResponses payload
      Just "request"  -> atomically $ writeTChan clientReverseRequests payload
      Just ty      -> assertFailure $ "handleServerTestDAP: Unsupported message type: " ++ show ty
      Nothing      -> assertFailure $ "Received message without type: " ++ show payload
  where
    nextPayload = do
      TestDAPClientContext{clientHandle = h} <- ask
      payload <- liftIO $ readPayload h
      case payload of
        Left e -> fail e
        Right actual -> pure actual

    parseType = withObject "message" $ \o -> do
      typ <- o .: "type"
      pure ((typ :: String))
