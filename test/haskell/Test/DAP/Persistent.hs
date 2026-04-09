{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Bifunctor

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
        , testGroup "cwd /= test_dir" $ do
            let units = replicate 2 ("test/unit/T113",(18,[]))
            [ testCase "sequential" $ testSequential' units [] $ simpleSessions'
             , testCase "parallel" $ testParallel' units [] $ simpleSessions'
             ]
        , testGroup "multiple test_dirs" $ do
            let units = concat $ replicate 2
                  [ ("test/unit/T113",(18,[]))
                  , ("test/unit/T44" ,(5,[eventMatch "output"]))
                  ]
            [  testCase "sequential" $ testSequential' units [] $ simpleSessions'
             , testCase "parallel" $ testParallel' units [] $ simpleSessions'
             ]
        ]
    ]


withServerTestSetup :: [String] -> (FilePath -> TestDAPServer -> IO a) -> IO a
withServerTestSetup flags check = do
  withHermeticDir False "test/unit/T113" $ \test_dir ->
    bracket (startTestDAPServer test_dir flags)
      (\server -> P.terminateProcess (testDAPServerProcess server))
      (check test_dir)

withServerTestSetup' :: [FilePath] -> [String] -> ([FilePath] -> TestDAPServer -> IO a) -> IO a
withServerTestSetup' dirs0 flags check = bracket (startTestDAPServer "." flags)
      (\server -> P.terminateProcess (testDAPServerProcess server))
      (go [] dirs0)

  where
    go acc [] server = check (reverse acc) server
    go acc (d:ds) server = withHermeticDir False d $ \test_dir ->
      go (test_dir:acc) ds server

withBreakPoints :: [(Int, Maybe String, Maybe String)] -> TestDAP a -> (FilePath, TestDAPServer) -> IO ()
withBreakPoints bps check (test_dir,server) =
     withTestDAPServerClient' server $ do
      () <- setupBreakpoints test_dir $ bps
      _ <- check

      -- Send disconnect
      disconnectSession
      pure ()

testSequential :: Foldable t => [String] -> ((FilePath, TestDAPServer) -> t (IO a)) -> IO ()
testSequential flags k
  = withServerTestSetup flags
      (curry $ \ x -> sequence_ $ k x)

testSequential' :: Foldable t =>
  [(FilePath,(Int,[MessageMatch]))] ->
  [String] ->
  ([(Int,[MessageMatch])] -> ([FilePath], TestDAPServer) -> t (IO a)) ->
  IO ()
testSequential' (unzip -> (dirs,bps)) flags k
  = withServerTestSetup' dirs flags
      (curry $ \ x -> sequence_ $ k bps x)

testParallel :: Foldable f => [String] -> ((FilePath, TestDAPServer) -> f (IO b)) -> IO ()
testParallel flags k
  = withServerTestSetup flags
      (curry $ \ x -> mapConcurrently_ id (k x))

testParallel' :: Foldable f =>
  [(FilePath,(Int,[MessageMatch]))] ->
  [String] ->
  ([(Int,[MessageMatch])] -> ([FilePath], TestDAPServer) -> f (IO b)) ->
  IO ()
testParallel' (unzip -> (dirs,bps)) flags k
  = withServerTestSetup' dirs flags
      (curry $ \ x -> mapConcurrently_ id (k bps x))

simpleSessions :: Int -> (FilePath, TestDAPServer) -> [IO ()]
simpleSessions n x =
  [withBreakPoints [(18,Nothing,Just msg)] check x
      | i <- [(0::Int)..n]
      , let msg = "MSG_" ++ show i
      , let
          check = do
            vs <- receiveMessagesUnordered [eventMatch "output"]
            hasLogMsg msg vs
                  -- consume further prints and events
            expectMessagesUnordered $
              replicate 3 (eventMatch "output")
                ++ [eventMatch "terminated", eventMatch "exited"]

      ]

-- simpleSessions' :: [(Int,_)] -> ([FilePath], TestDAPServer) -> [IO ()]
simpleSessions' ls (dirs,server) =
  [withBreakPoints [ (line,Nothing,Just msg)
                   , (line+1,Nothing,Nothing)
                   ] check (d,server)
      | (i,((line,matches),d)) <- zip [(0::Int)..] $ zip ls dirs
      , let msg = "MSG_" ++ show i
      , let
          check = do
            vs <- receiveMessagesUnordered [eventMatch "output"]
            hasLogMsg msg vs
            expectMessagesUnordered $ matches ++ [eventMatch "stopped"]
      ]