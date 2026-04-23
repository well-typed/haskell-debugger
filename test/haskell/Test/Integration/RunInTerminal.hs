-- | 'runInTerminal' tests
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.Integration.RunInTerminal (runInTerminalTests) where

import Data.List (isInfixOf)
import System.FilePath
import System.IO
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
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
  withTestDAPServer "test/integration/T44" flags $ \ test_dir server -> do

    let out_path = test_dir </> ("runInTerm" <.> "out")
        err_path = test_dir </> ("runInTerm" <.> "err")

    withTestDAPServerClientWith True (\_ _ -> pure Nothing) server $ do

      ctx <- ask
      (rit_in, rit_p) <- liftIO $ snd <$>
        concurrently
          (runTestDAP (hitBreakpointWith (mkLaunchConfig test_dir "Main.hs") 6) ctx)
          (flip runTestDAP ctx $
            handleRunInTerminal $ \args -> do
              (ritEnv, ritArgs) <- liftIO $ wait args
              let invocation = T.unpack $ "/usr/bin/env " <> addRITEnv ritEnv <> " " <> T.unwords ritArgs
              -- Redirect the child's stdout/stderr to files so logs are
              -- persisted incrementally and survive test failures.
              out_h <- liftIO $ openFile out_path WriteMode
              err_h <- liftIO $ openFile err_path WriteMode
              liftIO $ hPutStrLn out_h ("Invocation: " ++ invocation)
              liftIO $ hPutStrLn err_h ("Invocation: " ++ invocation)
              liftIO $ hFlush out_h
              liftIO $ hFlush err_h
              (Just rit_in, Nothing, Nothing, rit_p)
                <- liftIO $ P.createProcess (P.shell invocation)
                    {P.cwd = Just test_dir, P.std_in = P.CreatePipe, P.std_out = P.UseHandle out_h, P.std_err = P.UseHandle err_h}
              Just rit_pid <- liftIO $ P.getPid rit_p
              pure ((rit_in, rit_p), fromIntegral rit_pid))

      -- Continue from "getLine" which will block waiting for input
      next

      let secret_in = "SOMETHING_SECRET"

      -- Time to write to the stdin of the rit process
      liftIO $ hSetBuffering rit_in LineBuffering
      liftIO $ hPutStrLn rit_in secret_in

      -- Only after writing should we receive the next "stopped" event
      waitFiltering_ EventTy "stopped"

      -- To next line, which should be the "putStrLn" after the "getLine"
      next

      -- Wait for the program to actually execute `print arg` and pause at the
      -- next line before disconnecting; without this wait, disconnect can kill
      -- the process before the output is flushed.
      waitFiltering_ EventTy "stopped"

      -- Send disconnect
      disconnect

      liftIO $ do
        -- Wait for the child to exit so all buffered output lands on disk,
        -- then read back the logs that were streamed to the files.
        _ <- P.waitForProcess rit_p
        out_str <- readFile out_path
        err_str <- readFile err_path

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
