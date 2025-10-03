-- | 'runInTerminal' tests
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
module Test.DAP.RunInTerminal (runInTerminalTests) where

import Data.Aeson
import System.IO
import System.FilePath
import qualified Data.Text as T
import qualified System.Process as P
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.List (isInfixOf)
import Test.Tasty
import Test.Tasty.HUnit

import DAP.Types
import DAP.Utils
import Test.Utils
import Test.DAP


runInTerminalTests =
  testGroup "DAP.RunInTerminal"
    [ testCase "runInTerminal works and can the proxy forwards the stdin correctly" runInTerminal1
    ]

rit_keep_tmp_dirs :: Bool
rit_keep_tmp_dirs = False

runInTerminal1 = do
  withHermeticDir rit_keep_tmp_dirs "test/unit/T44" $ \test_dir -> do
    -- Launch server process
    (Just hin, Just hout, _, p)
      <- P.createProcess (P.shell $ "hdb server --port " ++ show testPort)
          {P.cwd = Just test_dir, P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}
    
    -- Connect to the DAP server
    withNewClient $ \handle -> do

      -- Initialize
      sendDAPRequest handle CommandInitialize InitializeRequestArguments
        { adapterID = "haskell-debugger"
        , clientID = Just "mock-client"
        , clientName = Just "Mock Client"
        , columnsStartAt1 = True
        , linesStartAt1 = True
        , locale = Just "en"
        , pathFormat = Just Path
        , supportsArgsCanBeInterpretedByShell = True
        , supportsInvalidatedEvent = True
        , supportsMemoryEvent = True
        , supportsMemoryReferences = True
        , supportsProgressReporting = True
        , supportsRunInTerminalRequest = True
        , supportsStartDebuggingRequest = True
        , supportsVariablePaging = True
        , supportsVariableType = True
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
              }
          , setBreakpointsArgumentsBreakpoints = Just [SourceBreakpoint {sourceBreakpointLine = 6, sourceBreakpointColumn = Nothing, sourceBreakpointCondition = Nothing, sourceBreakpointHitCondition = Nothing, sourceBreakpointLogMessage = Nothing}]
          , setBreakpointsArgumentsLines = Just [6]
          , setBreakpointsArgumentsSourceModified = Just False
          }

      _ <- shouldReceive handle
            [ "command" .= ("setBreakpoints" :: String)
            , "success" .= True
            , "body" .= object
                [ "breakpoints" .=
                    [ object
                        [ "id"       .= (0 :: Int)
                        , "verified" .= True
                        , "line"     .= (6 :: Int)
                        ]
                    ]
                ]
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

      sendDAPRequest handle CommandConfigurationDone (Nothing :: Maybe Value)

      _ <- shouldReceive handle
            [ "command" .= ("configurationDone" :: String)
            , "success" .= True
            , "type"    .= ("response" :: String)
            ]

      -- The program should start running now, and hit the breakpoint
      _ <- shouldReceive handle
            ["type" .= ("event" :: String), "event" .= ("output" :: String)]

      _ <- shouldReceive handle
            [ "event" .= ("stopped" :: String)
            , "type"  .= ("event" :: String)
            , "body"  .= object
                [ "reason" .= ("breakpoint" :: String)
                -- , "allThreadsStopped" .= True
                ]
            ]

      -- Advance to "getLine" (the next line after the breakpoint)
      goToNextLine handle
      _ <- shouldReceive handle
            ["type" .= ("event" :: String), "event" .= ("stopped" :: String)]

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
            [ "event" .= ("exited" :: String)
            , "type"  .= ("event" :: String)
            , "body"  .= object
                [ "exitCode" .= (0 :: Int)
                ]
            ]
      -- Kill the rit process if it's still running
      P.terminateProcess rit_p

  where
    goToNextLine handle = do
      _ <- sendDAPRequest handle CommandNext (Just (NextArguments {nextArgumentsThreadId = 0}))
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
  toJSON = genericToJSONWithModifier
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
