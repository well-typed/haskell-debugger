{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Unit.DAP.Persistent (persistentTests) where

import Control.Concurrent.Async
import qualified Data.Text as T
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Utils
import Test.Unit.DAP.LogMessage (setupBreakpoints)
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
        , testGroup "cwd /= test_dir" $ do
            let units = replicate 2 ("test/unit/T113",18)
            [ testCase "sequential" $ testSequential' units [] $ simpleSessions'
             , testCase "parallel" $ testParallel' units [] $ simpleSessions'
             ]
        , testGroup "multiple test_dirs" $ do
            let units = concat $ replicate 2
                  [ ("test/unit/T113",18)
                  , ("test/unit/T44" ,5)
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
     withTestDAPServerClient' False server $ do
      () <- setupBreakpoints test_dir bps
      _ <- check
      disconnect
        -- FIXME: we re-send the terminated event in response to disconnect,
        -- even if we already sent it once before above. probably not very
        -- correct
      pure ()

testSequential :: Foldable t => [String] -> ((FilePath, TestDAPServer) -> t (IO a)) -> IO ()
testSequential flags k
  = withServerTestSetup flags
      (curry $ \ x -> sequence_ $ k x)

testSequential' :: Foldable t =>
  [(FilePath,Int)] ->
  [String] ->
  ([Int] -> ([FilePath], TestDAPServer) -> t (IO a)) ->
  IO ()
testSequential' (unzip -> (dirs,bps)) flags k
  = withServerTestSetup' dirs flags
      (curry $ \ x -> sequence_ $ k bps x)

testParallel :: Foldable f => [String] -> ((FilePath, TestDAPServer) -> f (IO b)) -> IO ()
testParallel flags k
  = withServerTestSetup flags
      (curry $ \ x -> mapConcurrently_ id (k x))

testParallel' :: Foldable f =>
  [(FilePath,Int)] ->
  [String] ->
  ([Int] -> ([FilePath], TestDAPServer) -> f (IO b)) ->
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
            assertOutput (T.pack msg)
            waitFiltering Event "exited"
      ]

simpleSessions' :: [Int] -> ([FilePath], TestDAPServer) -> [IO ()]
simpleSessions' ls (dirs,server) =
  [ withBreakPoints [ (line,Nothing,Just msg)
                    , (line+1,Nothing,Nothing)
                    ] check (d,server)
      | (i,(line,d)) <- zip [(0::Int)..] $ zip ls dirs
      , let msg = "MSG_" ++ show i
      , let
          check = do
            assertOutput (T.pack msg)
            waitFiltering Event "stopped"
      ]
