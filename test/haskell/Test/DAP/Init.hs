{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Init where

----------------------------------------------------------------------------
import Data.Maybe
import Data.List (isInfixOf)
import           Control.Exception hiding (handle)
import qualified Control.Exception as E
import           Network.Run.TCP
import           Network.Socket             (Family(AF_INET), SockAddr(SockAddrInet, SockAddrInet6), SocketOption(ReuseAddr), SocketType(Stream), bind, close, defaultProtocol, getSocketName, setSocketOption, socket, socketToHandle, tupleToHostAddress)
import           System.IO
import           Control.Retry
import           Data.IORef
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>))
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
import Test.Utils (withHermeticDir)
import DAP.Types (OutputEvent (..))
import Test.DAP.Messages.Parser

--------------------------------------------------------------------------------
-- * Launch the DAP server process (what we're testing)
--------------------------------------------------------------------------------

data TestDAPServer = TestDAPServer
  { testDAPServerPort :: Int
  , testDAPServerCleanup :: IO ()
  , testDAPServerFlushOutput :: IO ()
  }

-- | Launch an @hdb server@ for tests on a random local port and capture stdout.
--   Prefer withTestDAPServer because it will terminate the process at the end.
startTestDAPServer :: FilePath -> [String] -> IO TestDAPServer
startTestDAPServer testDir flags = do
  testPort <- getAvailablePort
  let nameTemplate= testDir </> ("server_port_" ++ show testPort)
  let openHandle ext = do
        h <- openFile (nameTemplate <.> ext) WriteMode
        hSetBuffering h LineBuffering
        pure h
  hout <- openHandle "out"
  herr <- openHandle "err"
  (Just hin, Nothing, Nothing, p)
    <- P.createProcess (P.proc "hdb" $ ["server"] ++ flags ++ ["--port", show testPort])
        { P.cwd = Just testDir
        , P.std_out = P.UseHandle hout
        , P.std_err = P.UseHandle herr
        , P.std_in = P.CreatePipe
        }

  pid <- fromMaybe 0 <$> P.getPid p
  writeFile (nameTemplate <.> "pid") (show pid)

  let flushServerOutput = do
        putStrLn "\n--- SERVER OUTPUT ---"
        putStrLn $ "See: " ++ testDir
        putStrLn $ "Might need: KEEP_TEMP_DIRS=True"
        putStrLn "---------------------\n"

  pure TestDAPServer
    { testDAPServerPort = testPort
    , testDAPServerFlushOutput = flushServerOutput
    , testDAPServerCleanup = do
        P.cleanupProcess (Just hin, Just hout, Just herr, p)

    }

-- | Prefer this to startTestDAPServer
withTestDAPServer :: FilePath -> [String] -> (FilePath -> TestDAPServer -> IO a) -> IO a
withTestDAPServer dir flags check' = do
  keep_tmp_dirs <- maybe False read <$> lookupEnv "KEEP_TEMP_DIRS"
  withHermeticDir keep_tmp_dirs dir $ \test_dir ->
    bracket (startTestDAPServer test_dir flags)
      testDAPServerCleanup
      (check' test_dir)

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

-- | Like 'withTestDAPServerClientWith' but default:
--  - @RunInTerminal = False@
--  - @clientHandleNoSuccess = \_ _ -> pure Nothing@ (by default, success: false FAILS test)
withTestDAPServerClient :: TestDAPServer -> TestDAP a -> IO a
withTestDAPServerClient = withTestDAPServerClientWith False (\_ _ -> pure Nothing)

--- | Connect a test client to a running 'TestDAPServer', with retry semantics
--- and server log flushing on failure.
withTestDAPServerClientWith :: Bool {-^ Announce support for runInTerminal? -} -> (String -> Value -> IO (Maybe Value))
                            -> TestDAPServer -> TestDAP a -> IO a
withTestDAPServerClientWith clientSupportsRunInTerminal clientHandleNoSuccess server continue = do
  runClient `E.onException` testDAPServerFlushOutput server
  where
    runClient = do
      withNewClient (testDAPServerPort server) $ \clientHandle -> do
        clientNextSeqRef           <- newIORef 1
        clientReverseRequests      <- newTChanIO
        clientResponses            <- newTChanIO
        clientEvents               <- newTChanIO
        clientFullOutput           <- newTVarIO []
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
      [const $ Control.Monad.Catch.Handler $ \ (e :: IOException) -> return $ "Network.Socket.connect" `isInfixOf` show e]

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
      Just "event"    -> do
        let mtxt = fromJSON @(Event OutputEvent) payload
        atomically $ do
          writeTChan clientEvents payload
          case mtxt of
            Success (Event _ (Just txt)) -> modifyTVar' clientFullOutput (outputEventOutput txt:)
            _ -> pure ()
      Just "response" ->
        -- Fail immediately if the server reports failure, even if the test
        -- is blocked waiting for some other specific message --
        -- unless the test has opted into handling failed responses itself.
        case parseMaybe parseSuccess payload of
          Just errMsg -> clientHandleNoSuccess errMsg payload >>= \case
            Just v    -> atomically $ writeTChan clientResponses v
            Nothing   -> assertFailure errMsg
          Nothing     -> atomically $ writeTChan clientResponses payload
      Just "request"  -> atomically $ do writeTChan clientReverseRequests payload
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

    -- Returns an error message when success is False, Nothing when success is True.
    parseSuccess = withObject "response" $ \o -> do
      success <- o .: "success"
      if success
        then fail "success"
        else do
          msg <- o .:? "message" .!= "DAP response had success: false (no message)"
          pure (msg :: String)
