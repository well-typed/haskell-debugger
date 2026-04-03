-- | 'runInTerminal' tests
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.DAP.RunInTerminal (runInTerminalTests) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.List (isInfixOf)
import System.IO
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
      ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
      testGroup "runInTerminal: proxy forwards stdin correctly"
        [ testCase "(default)" (runInTerminal1 [])
        , testCase "(--internal-interpreter)" (runInTerminal1 ["--internal-interpreter"])
        ]
    ]

rit_keep_tmp_dirs :: Bool
rit_keep_tmp_dirs = False

runInTerminal1 flags = do
  withHermeticDir rit_keep_tmp_dirs "test/unit/T44" $ \test_dir -> do

    server <- startTestDAPServer test_dir flags

    withTestDAPServerClient server $ do

      (rit_in, rit_out, rit_p) <-
        hitBreakpoint True test_dir 6
          (do
            -- Receive a runInTerminal request!!
            (ritEnv, ritArgs) <- receiveRunInTerminal
            (Just rit_in, Just rit_out, _, rit_p)
              <- liftIO $ P.createProcess
                (P.shell $ T.unpack $
                    "/usr/bin/env " <> addRITEnv ritEnv <> " " <> T.unwords ritArgs)
                  {P.cwd = Just test_dir, P.std_in = P.CreatePipe, P.std_out = P.CreatePipe}

            pure (rit_in, rit_out, rit_p)
          )
          (\(_, _, rit_p) -> do
            Just rit_pid <- liftIO $ P.getPid rit_p
            sendRunInTerminalResponse (fromIntegral rit_pid :: Int)
          )

      -- Continue from "getLine" which will block waiting for input
      stepNextLine

      let secret_in = "SOMETHING_SECRET"

      -- Time to write to the stdin of the rit process
      liftIO $ hSetBuffering rit_in LineBuffering
      liftIO $ hPutStrLn rit_in secret_in

      -- Only after writing should we receive the next "stopped" event
      expectMessagesUnordered [eventMatch "stopped"]

      -- To next line, which should be the "putStrLn" after the "getLine"
      stepNextLine
      -- It's both stopped and we receive the SOMETHING_SECRET printed out. Order not important.
      expectMessagesUnordered $
        replicate 2 (subsetMatch ["type" .= ("event" :: String)])

      -- The contents of the rit_output should contain "hello" plus printing of what we wrote
      out <- liftIO $ LBS.hGetContents rit_out
      let out_str = LB8.unpack out
      liftIO $ assertBool ("Expected output to contain 'hello', got: " ++ show out_str)
                 ("hello" `isInfixOf` out_str)
      liftIO $ assertBool ("Expected output to contain '" ++ secret_in ++ "' , got: " ++ show out_str)
                 (secret_in `isInfixOf` out_str)

      -- Send disconnect
      disconnectSession

      -- Kill the process
      liftIO $ P.terminateProcess rit_p

  where
    addRITEnv :: Maybe (H.HashMap T.Text T.Text) -> T.Text
    addRITEnv env =
      case env of
        Nothing -> ""
        Just env -> T.unwords [k{-todo: escape-} <> "=" <> v | (k,v) <- H.toList env]
