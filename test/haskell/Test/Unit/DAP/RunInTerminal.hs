-- | 'runInTerminal' tests
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.Unit.DAP.RunInTerminal (runInTerminalTests) where

import Data.List (isInfixOf)
import System.FilePath
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
import Control.Concurrent.Async
import Control.Monad.Reader

runInTerminalTests :: TestTree
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

runInTerminal1 :: [String] -> IO ()
runInTerminal1 flags = do
  withTestDAPServer "test/unit/T44" flags $ \ test_dir server -> do

    withTestDAPServerClient True server $ do

      ctx <- ask
      (rit_in, rit_out, rit_err, rit_p, invocation) <- liftIO $ snd <$>
        concurrently
          (runTestDAP (defaultHitBreakpoint test_dir 6) ctx)
          (flip runTestDAP ctx $
            handleRunInTerminal $ \args -> do
              (ritEnv, ritArgs) <- liftIO $ wait args
              let invocation = T.unpack $ "/usr/bin/env " <> addRITEnv ritEnv <> " " <> T.unwords ritArgs
              (Just rit_in, Just rit_out, Just rit_err, rit_p)
                <- liftIO $ P.createProcess (P.shell invocation)
                    {P.cwd = Just test_dir, P.std_in = P.CreatePipe, P.std_out = P.CreatePipe, P.std_err = P.CreatePipe}
              Just rit_pid <- liftIO $ P.getPid rit_p
              pure ((rit_in, rit_out, rit_err, rit_p, invocation), fromIntegral rit_pid))

      -- Continue from "getLine" which will block waiting for input
      next

      let secret_in = "SOMETHING_SECRET"

      -- Time to write to the stdin of the rit process
      liftIO $ hSetBuffering rit_in LineBuffering
      liftIO $ hPutStrLn rit_in secret_in

      -- Only after writing should we receive the next "stopped" event
      _ <- waitFiltering Event "stopped"

      -- To next line, which should be the "putStrLn" after the "getLine"
      next

      -- Send disconnect
      disconnect

      liftIO $ do
        -- The contents of the rit_output should contain "hello" plus printing of what we wrote
        out <- LBS.hGetContents rit_out
        let out_str = LB8.unpack out

        -- Disconnect should be handled cleanly (#268)
        err <- LBS.hGetContents rit_err
        let err_str = LB8.unpack err

        -- Write stdout and stderr to file.
        writeFile (test_dir </> ("runInTerm" <.> "out")) (invocation ++ "\n" ++ out_str)
        writeFile (test_dir </> ("runInTerm" <.> "err")) (invocation ++ "\n" ++ err_str)

        assertBool
          ("Expected output to contain 'hello', got: " ++ out_str)
          ("hello" `isInfixOf` out_str)
        assertBool
          ("Expected output to contain '" ++ secret_in ++ "' , got: " ++ out_str)
          (secret_in `isInfixOf` out_str)

        assertBool
          ("The stderr of the runInTerminal process shouldn't have any errors, but has: " ++ err_str ++ "\nStdout: " ++ out_str)
          (not ("GHCi.Message.readPipe:" `isInfixOf` err_str) && not ("Uncaught exception" `isInfixOf` err_str))

        -- -- Kill the process
        P.terminateProcess rit_p

  where
    addRITEnv :: Maybe (H.HashMap T.Text T.Text) -> T.Text
    addRITEnv env =
      case env of
        Nothing -> ""
        Just ev -> T.unwords [k{-todo: escape-} <> "=" <> v | (k,v) <- H.toList ev]
