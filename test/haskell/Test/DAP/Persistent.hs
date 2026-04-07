{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.DAP.Persistent (persistentTests) where

import Control.Concurrent.Async
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Utils
import Test.DAP.LogMessage (hasLogMsg,setupBreakpoints)
import qualified System.Process as P
import Control.Exception (bracket)

persistentTests :: TestTree
persistentTests =
#ifdef mingw32_HOST_OS
 ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Persistent"
    [
      testGroup "multiple sessions"
        [ testCase "sequential" $
          testSequential [] $ simpleSessions 2
        , testCase "parallel" $
          testParallel [] $ simpleSessions 2
        ]
    ]


withServerTestSetup :: [String] -> (FilePath -> TestDAPServer -> IO a) -> IO a
withServerTestSetup flags check = do
  withHermeticDir False "test/unit/T113" $ \test_dir ->
    bracket (startTestDAPServer test_dir flags)
      (\server -> P.terminateProcess (testDAPServerProcess server))
      (check test_dir)

withBreakPoints :: [(Int, Maybe String, Maybe String)] -> TestDAP a -> (FilePath, TestDAPServer) -> IO ()
withBreakPoints bps check (test_dir,server) =
     withTestDAPServerClient' server $ do
      () <- setupBreakpoints test_dir $ bps
      _ <- check

      -- consume further prints and events
      expectMessagesUnordered $
        replicate 3 (eventMatch "output")
          ++ [eventMatch "terminated", eventMatch "exited"]

      -- Send disconnect
      disconnectSession
      pure ()

testSequential :: Foldable t => [String] -> ((FilePath, TestDAPServer) -> t (IO a)) -> IO ()
testSequential flags k = withServerTestSetup flags (curry $ \ x -> sequence_ $ k x)

testParallel :: Foldable f => [String] -> ((FilePath, TestDAPServer) -> f (IO b)) -> IO ()
testParallel flags k = withServerTestSetup flags (curry $ \ x -> mapConcurrently_ id (k x))

simpleSessions :: Int -> (FilePath, TestDAPServer) -> [IO ()]
simpleSessions n x =
  [withBreakPoints [(18,Nothing,Just msg)] check x
      | i <- [0..n]
      , let msg = "MSG_" ++ show i
      , let
          check = do
            vs <- receiveMessagesUnordered [eventMatch "output"]
            hasLogMsg msg vs
      ]