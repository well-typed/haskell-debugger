-- | 'runInTerminal' tests
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.DAP.RunInTerminal (runInTerminalTests) where

import Control.Concurrent
import DAP.Types
import DAP.Utils
import Data.Aeson
import Data.IORef
import Data.List (isInfixOf)
import System.FilePath
import System.IO
import System.Random
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Utils
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified System.Process as P

runInTerminalTests =
  testGroup "DAP.RunInTerminal"
    [
#ifdef mingw32_HOST_OS
      ignoreTestBecause "Needs to be fixed for Windows" $
#endif
      testGroup "runInTerminal: proxy forwards stdin correctly"
        [ testCase "(default)" (runInTerminal1 "")
        , testCase "(--internal-interpreter)" (runInTerminal1 "--internal-interpreter")
        ]
    ]

rit_keep_tmp_dirs :: Bool
rit_keep_tmp_dirs = False

runInTerminal1 flags = do
  withHermeticDir rit_keep_tmp_dirs "test/unit/T44" $ \test_dir -> do

    -- Come up with a random port
    testPort <- randomRIO (49152, 65534) :: IO Int

    -- Launch server process
    (Just hin, Just hout, _, p)
      <- P.createProcess (P.shell $ "hdb server " ++ flags ++ " --port " ++ show testPort)
          {P.cwd = Just test_dir, P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}

    -- Fork thread to print out output of server process
    -- This is surprisingly needed, otherwise the server process
    -- will be broken, perhaps because it blocks trying to write to stdout/stderr if the buffer is full?
    forkIO $ do
      hSetBuffering hout LineBuffering
      let loop = do
            eof <- hIsEOF hout
            if eof
              then return ()
              else do
                _l <- hGetLine hout
                -- UNCOMMENT ME TO DEBUG
                -- putStrLn ("[server] " ++ _l)
                loop
      loop

    retryVar <- newIORef True
    -- Connect to the DAP server
    withNewClient testPort retryVar $ \handle -> do
      -- As soon as we get a connection, stop retrying
      writeIORef retryVar False

      -- Initialize
      sendDAPRequest handle CommandInitialize InitializeRequestArguments
        { adapterID = "haskell-debugger"
        , clientID = Just "mock-client"
        , clientName = Just "Mock Client"
        , columnsStartAt1 = Just True
        , linesStartAt1 = Just True
        , locale = Just "en"
        , pathFormat = Just Path
        , supportsArgsCanBeInterpretedByShell = Nothing
        , supportsInvalidatedEvent = Nothing
        , supportsMemoryEvent = Nothing
        , supportsMemoryReferences = Nothing
        , supportsProgressReporting = Nothing
        , supportsRunInTerminalRequest = Just True
        , supportsStartDebuggingRequest = Nothing
        , supportsVariablePaging = Nothing
        , supportsVariableType = Nothing
        }

      -- Recv initalize response
      _ <- shouldReceive handle []

      -- Send launch request
      send handle
        [ "command"  .= ("launch" :: String)
        , "seq"      .= (2 :: Int)
        , "type"     .= ("request" :: String)
        , "arguments".= object
            [ "entryFile" .= (test_dir </> "Main.hs" :: String)
            , "entryPoint" .= ("main" :: String)
            , "projectRoot" .= (test_dir :: String)
            , "extraGhcArgs" .= ([] :: [String])
            , "entryArgs" .= ([] :: [String])
            , "request" .= ("launch" :: String)
            ]
        ]

      _ <- shouldReceive handle
            ["type" .= ("event" :: String), "event" .= ("output" :: String)]
      _ <- shouldReceive handle
            ["type" .= ("event" :: String), "event" .= ("output" :: String)]
      _ <- shouldReceive handle
            ["type" .= ("event" :: String), "event" .= ("output" :: String)]
      _ <- shouldReceive handle
            [ "command" .= ("launch" :: String)
            , "success" .= True]
      _ <- shouldReceive handle
            [ "event" .= ("initialized" :: String)
            , "type"  .= ("event" :: String)
            ]

      -- Receive a runInTerminal request!!
      r@RunInTerminalRequestArguments{} <- recvDAPResponse handle
      (Just rit_in, Just rit_out, _, rit_p)
        <- P.createProcess
          (P.shell $ T.unpack $
              "/usr/bin/env " <> addRITEnv r.runInTerminalRequestArgumentsEnv <> " " <> T.unwords (r.runInTerminalRequestArgumentsArgs))
            {P.cwd = Just test_dir, P.std_in = P.CreatePipe, P.std_out = P.CreatePipe}

      -- Send a breakpoint request
      sendDAPRequest handle CommandSetBreakpoints
        SetBreakpointsArguments
          { setBreakpointsArgumentsSource = Source
              { sourceName = Just "Main.hs"
              , sourcePath = Just $ T.pack $ test_dir </> "Main.hs"
              , sourceSourceReference = Nothing
              , sourcePresentationHint = Nothing
              , sourceOrigin = Nothing
              , sourceAdapterData = Nothing
              , sourceChecksums = Nothing
              , sourceSources = Nothing
              }
          , setBreakpointsArgumentsBreakpoints = Just [SourceBreakpoint {sourceBreakpointLine = 6, sourceBreakpointColumn = Nothing, sourceBreakpointCondition = Nothing, sourceBreakpointHitCondition = Nothing, sourceBreakpointLogMessage = Nothing}]
          , setBreakpointsArgumentsLines = Just [6]
          , setBreakpointsArgumentsSourceModified = Just False
          }

      _ <- shouldReceive handle
            [ "command" .= ("setBreakpoints" :: String)
            , "success" .= True
            , "type"    .= ("response" :: String)
            ]

      -- Send runInTerminal response
      Just rit_pid <- P.getPid rit_p
      send handle
        [ "command" .= ("runInTerminal" :: String)
        , "seq"     .= (6 :: Int)
        , "type"    .= ("response" :: String)
        , "success" .= True
        , "body"    .= object
            [ "shellProcessId" .= (fromIntegral rit_pid :: Int)
            ]
        ]

      send handle
        [ "seq"      .= (1 :: Int)
        , "type"     .= ("request" :: String)
        , "command"  .= CommandConfigurationDone
        ]

      _ <- shouldReceive handle
            [ "command" .= ("configurationDone" :: String)
            , "success" .= True
            , "type"    .= ("response" :: String)
            ]

      -- The program should start running now, and hit the breakpoint.
      -- It will also print "Hello". Since the order is not important, we just
      -- match on the type == event and ignore if it's "stopped" or "output"
      _ <- shouldReceive handle
            [ "type"  .= ("event" :: String) ]
      _ <- shouldReceive handle
            [ "type"  .= ("event" :: String) ]

      -- Continue from "getLine" which will block waiting for input
      goToNextLine handle

      let secret_in = "SOMETHING_SECRET"

      -- Time to write to the stdin of the rit process
      hSetBuffering rit_in LineBuffering
      hPutStrLn rit_in secret_in

      -- Only after writing should we receive the next "stopped" event
      _ <- shouldReceive handle
            ["type" .= ("event" :: String), "event" .= ("stopped" :: String)]

      -- To next line, which should be the "putStrLn" after the "getLine"
      goToNextLine handle
      -- It's both stopped and we receive the SOMETHING_SECRET printed out. Order not important.
      _ <- shouldReceive handle
            ["type" .= ("event" :: String)]
      _ <- shouldReceive handle
            ["type" .= ("event" :: String)]

      -- The contents of the rit_output should contain "hello" plus printing of what we wrote
      out <- LBS.hGetContents rit_out
      let out_str = LB8.unpack out
      assertBool ("Expected output to contain 'hello', got: " ++ out_str)
                 ("hello" `isInfixOf` out_str)
      assertBool ("Expected output to contain '" ++ secret_in ++ "' , got: " ++ out_str)
                 (secret_in `isInfixOf` out_str)

      -- Send disconnect
      sendDAPRequest handle CommandDisconnect (DisconnectArguments {disconnectArgumentsRestart = False, disconnectArgumentsTerminateDebuggee = True, disconnectArgumentsSuspendDebuggee = False})
      _ <- shouldReceive handle
            [ "command" .= ("disconnect" :: String)
            , "success" .= True
            , "type"    .= ("response" :: String)
            ]
      _ <- shouldReceive handle
            [ "event" .= ("terminated" :: String)
            , "type"  .= ("event" :: String)
            ]
      -- Kill the processes if they're still running
      P.terminateProcess rit_p
      P.terminateProcess p

  where
    goToNextLine handle = do
      _ <- sendDAPRequest handle CommandNext (Just (NextArguments {nextArgumentsThreadId = 0, nextArgumentsSingleThread = Nothing, nextArgumentsGranularity = Nothing}))
      _ <- shouldReceive handle
            [ "command" .= ("next" :: String)
            , "success" .= True
            , "type"    .= ("response" :: String)
            ]
      return ()

    addRITEnv :: Maybe (H.HashMap T.Text T.Text) -> T.Text
    addRITEnv env =
      case env of
        Nothing -> ""
        Just env -> T.unwords [k{-todo: escape-} <> "=" <> v | (k,v) <- H.toList env]
--------------------------------------------------------------------------------
instance ToJSON InitializeRequestArguments where
  toJSON InitializeRequestArguments{..} = object
    [ "adapterID" .= adapterID
    , "clientID" .= clientID
    , "clientName" .= clientName
    , "columnsStartAt1" .= columnsStartAt1
    , "linesStartAt1" .= linesStartAt1
    , "locale" .= locale
    , "pathFormat" .= pathFormat
    , "supportsArgsCanBeInterpretedByShell" .= supportsArgsCanBeInterpretedByShell
    , "supportsInvalidatedEvent" .= supportsInvalidatedEvent
    , "supportsMemoryEvent" .= supportsMemoryEvent
    , "supportsMemoryReferences" .= supportsMemoryReferences
    , "supportsProgressReporting" .= supportsProgressReporting
    , "supportsRunInTerminalRequest" .= supportsRunInTerminalRequest
    , "supportsStartDebuggingRequest" .= supportsStartDebuggingRequest
    , "supportsVariablePaging" .= supportsVariablePaging
    , "supportsVariableType" .= supportsVariableType
    ]
instance ToJSON PathFormat where
  toJSON Path = String "path"
  toJSON URI  = String "uri"
  toJSON (PathFormat x) = String x
instance ToJSON SetBreakpointsArguments where
  toJSON = genericToJSONWithModifier
instance ToJSON NextArguments where
  toJSON = genericToJSONWithModifier
instance ToJSON DisconnectArguments where
  toJSON = genericToJSONWithModifier
instance ToJSON SourceBreakpoint where
  toJSON = genericToJSONWithModifier
instance ToJSON SteppingGranularity where
  toJSON = genericToJSONWithModifier
instance FromJSON bps => FromJSON (Breakpoints bps) where
  parseJSON = withObject "bkrps" $ \o ->
    Breakpoints <$> o .: "breakpoints"
instance FromJSON Breakpoint where
  parseJSON = genericParseJSONWithModifier

