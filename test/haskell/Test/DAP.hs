{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP
  ( module Test.DAP
  , module Test.DAP.Init
  , module Test.DAP.Messages
  ) where

----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.KeyMap (toHashMapText)
import qualified Data.Foldable as F
import           Control.Monad.Reader
import           Control.Exception hiding (handle)
import qualified Control.Exception as E
import qualified Data.ByteString            as BS
import qualified Data.HashMap.Strict as H
import           Data.List (findIndex, sortOn)
import           Network.Run.TCP
import           Network.Socket             (socketToHandle)
import           System.IO
import           System.Exit
import           Data.IORef
import           System.FilePath ((</>))
import           System.Random (randomRIO)
import qualified System.Process as P
import qualified Data.Text as T
----------------------------------------------------------------------------
import           DAP.Utils
import           DAP.Server
----------------------------------------------------------------------------
import           Test.Tasty.HUnit
----------------------------------------------------------------------------
import           Test.DAP.Init
import           Test.DAP.Messages
import           Test.DAP.Parser

-- | Launches the debuggee and runs until the given breakpoint is hit.
--
-- The first hook is run after launch/initialized and before setBreakpoints.
-- The second hook is run after setBreakpoints is acknowledged and before
-- configurationDone is sent (used by tests that need extra handshake steps).
hitBreakpoint :: Bool -> FilePath -> Int -> TestDAP a -> (a -> TestDAP ()) -> TestDAP a
hitBreakpoint supportsRunInTerminal testDir line beforeSetBreakpoints beforeConfigurationDone = do
  sendInitialize supportsRunInTerminal
  expectMessagesUnordered [responseMatch "initialize"]

  sendLaunch testDir
  expectMessagesUnordered $
#if __GLASGOW_HASKELL__ >= 915
    -- GHC 9.15 doesn't produce "[2 of 3] Compiling Main ..." for some reason
    replicate 8 (eventMatch "output")
#else
    replicate 9 (eventMatch "output")
#endif
      ++ [ responseMatch "launch"
         , eventMatch "initialized"
         ]

  hookResult <- beforeSetBreakpoints

  sendSetBreakpoints testDir line
  expectMessagesUnordered [responseMatch "setBreakpoints"]

  beforeConfigurationDone hookResult

  sendConfigurationDone
  expectMessagesUnordered
    [ responseMatch "configurationDone"
    , eventMatch "stopped"
    , subsetMatch ["type" .= ("event" :: String)]
    ]

  pure hookResult

stepNextLine :: TestDAP ()
stepNextLine = do
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("next" :: String)
    , "arguments" .= object
      [ "threadId" .= (0 :: Int)
      ]
    ]
  expectMessagesUnordered [responseMatch "next"]
  pure ()

-- | Request threads and parse the first thread id.
getThreads :: TestDAP Int
getThreads = do
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("threads" :: String)
    ]
  threadsResp <- waitForResponseIgnoringOutput "threads"
  case parseThreadId threadsResp of
    Nothing -> fail $ "Could not parse first thread id from: " ++ show threadsResp
    Just tid -> pure tid

-- | Request stackTrace and parse the first frame id.
getStackTrace :: Int -> TestDAP Int
getStackTrace threadId = do
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("stackTrace" :: String)
    , "arguments" .= object
      [ "threadId" .= threadId
      ]
    ]
  stackResp <- waitForResponseIgnoringOutput "stackTrace"
  case parseFrameId stackResp of
    Nothing -> fail $ "Could not parse first frame id from: " ++ show stackResp
    Just fid -> pure fid

-- | Request scopes and parse (name, expensive) pairs.
getScopes :: Int -> TestDAP [(String, Bool)]
getScopes frameId = do
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("scopes" :: String)
    , "arguments" .= object
      [ "frameId" .= frameId
      ]
    ]
  scopesResp <- waitForResponseIgnoringOutput "scopes"
  case parseScopes scopesResp of
    Nothing -> fail $ "Could not parse scopes from: " ++ show scopesResp
    Just scopes -> pure scopes

waitForResponseIgnoringOutput :: String -> TestDAP Value
waitForResponseIgnoringOutput commandName = do
  TestDAPClientContext{clientHandle = h} <- ask
  payload <- liftIO $ readPayload h
  actual <- case payload of
    Left e -> fail e
    Right v -> pure v

  if messageMatchMatches (responseMatch commandName) actual
    then pure actual
    else
      if messageMatchMatches (eventMatch "output") actual
        then waitForResponseIgnoringOutput commandName
        else fail $
          "Unexpected message while waiting for response(" ++ commandName ++ "): " ++ show actual

disconnectSession :: TestDAP ()
disconnectSession = do
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("disconnect" :: String)
    , "arguments" .= object
      [ "restart" .= False
      , "terminateDebuggee" .= True
      , "suspendDebuggee" .= False
      ]
    ]
  expectMessagesUnordered
    [ responseMatch "disconnect"
    , eventMatch "terminated"
    ]

--------------------------------------------------------------------------------
-- * runInTerminal
--------------------------------------------------------------------------------

receiveRunInTerminal :: TestDAP (Maybe (H.HashMap T.Text T.Text), [T.Text])
receiveRunInTerminal = do
  TestDAPClientContext{clientHandle = h} <- ask
  payload <- liftIO $ readPayload h
  actual <- case payload of
    Left e -> fail e
    Right v -> pure v
  case parseMaybe (withObject "runInTerminal request" $ \o -> do
          ("request" :: String) <- o .: "type"
          ("runInTerminal" :: String) <- o .: "command"
          argsObj <- o .: "arguments"
          withObject "runInTerminal arguments" (\a -> do
            env <- a .:? "env"
            args <- a .: "args"
            pure (env, args)
            ) argsObj
        ) actual of
    Nothing -> fail $ "Failed to parse runInTerminal request: " ++ show actual
    Just req -> pure req

sendRunInTerminalResponse :: Int -> TestDAP ()
sendRunInTerminalResponse shellProcessId =
  send
    [ "command" .= ("runInTerminal" :: String)
    , "type"    .= ("response" :: String)
    , "success" .= True
    , "body"    .= object
        [ "shellProcessId" .= shellProcessId
        ]
    ]

--------------------------------------------------------------------------------
-- * Lower level interface
--------------------------------------------------------------------------------

sendInitialize :: Bool -> TestDAP ()
sendInitialize supportsRunInTerminal =
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("initialize" :: String)
    , "arguments" .= object
      [ "adapterID" .= ("haskell-debugger" :: String)
      , "clientID" .= ("mock-client" :: String)
      , "clientName" .= ("Mock Client" :: String)
      , "columnsStartAt1" .= True
      , "linesStartAt1" .= True
      , "locale" .= ("en" :: String)
      , "pathFormat" .= ("path" :: String)
      , "supportsRunInTerminalRequest" .= supportsRunInTerminal
      ]
    ]

sendLaunch :: FilePath -> TestDAP ()
sendLaunch testDir =
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("launch" :: String)
    , "arguments" .= object
      [ "entryFile" .= (testDir </> "Main.hs")
      , "entryPoint" .= ("main" :: String)
      , "projectRoot" .= testDir
      , "extraGhcArgs" .= ([] :: [String])
      , "entryArgs" .= ([] :: [String])
      , "request" .= ("launch" :: String)
      ]
    ]

sendConfigurationDone :: TestDAP ()
sendConfigurationDone =
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("configurationDone" :: String)
    ]

sendSetBreakpoints :: FilePath -> Int -> TestDAP ()
sendSetBreakpoints testDir line = sendSetBreakpoints' testDir [(line, Nothing,Nothing)]

sendSetBreakpoints' :: FilePath -> [(Int, Maybe String, Maybe String)] -> TestDAP ()
sendSetBreakpoints' testDir bps =
  send
    [ "type" .= ("request" :: String)
    , "command" .= ("setBreakpoints" :: String)
    , "arguments" .= object
      [ "source" .= object
        [ "name" .= ("Main.hs" :: String)
        , "path" .= T.pack (testDir </> "Main.hs")
        ]
      , "breakpoints" .=
        [ object
          [ "line" .= line
          , "logMessage" .= logMessage
          , "condition" .= condition
          ]
        | (line,condition, logMessage) <- bps
        ]
      , "lines" .= [line | (line,_,_) <- bps ]
      , "sourceModified" .= False
      ]
    ]
