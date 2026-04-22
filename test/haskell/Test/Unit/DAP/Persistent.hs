{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Unit.DAP.Persistent (persistentTests) where

import Control.Concurrent.Async
import qualified Data.Text as T
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils
import Test.Unit.DAP.LogMessage (mkBP)
import qualified DAP
import Control.Exception (bracket)
import System.Environment (lookupEnv)

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


withServerTestSetup' :: [FilePath] -> [String] -> ([FilePath] -> TestDAPServer -> IO a) -> IO a
withServerTestSetup' [] _ _ = error "no test dirs"
withServerTestSetup' dirs0@(d0:_) flags check = do
  keep_temp_dirs <- maybe False read <$> lookupEnv "KEEP_TEMP_DIRS"
  withHermeticDir keep_temp_dirs d0 $ \server_dir ->
    bracket (startTestDAPServer server_dir flags)
      (testDAPServerCleanup)
      (go keep_temp_dirs [] dirs0)

  where
    go _keep acc [] server = check (reverse acc) server
    go keep acc (d:ds) server = withHermeticDir keep d $ \test_dir ->
      go keep (test_dir:acc) ds server

withBreakPoints :: [DAP.SourceBreakpoint] -> TestDAP () -> (FilePath, TestDAPServer) -> IO ()
withBreakPoints bps check (test_dir, server) =
  withTestDAPServerClient server $ do
    _ <- sync $ launchWith (mkLaunchConfig test_dir "Main.hs")
    waitFiltering_ EventTy "initialized"
    _ <- sync $ setBreakpointsIn test_dir "Main.hs" bps
    _ <- sync configurationDone
    _ <- check
    disconnect

testSequential :: Foldable t => [String] -> ((FilePath, TestDAPServer) -> t (IO a)) -> IO ()
testSequential flags k
  = withTestDAPServer "test/unit/T113" flags
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
  = withTestDAPServer "test/unit/T113" flags
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
  [withBreakPoints [mkBP 18 Nothing (Just msg)] check x
      | i <- [(0::Int)..n]
      , let msg = "MSG_" ++ show i
      , let
          check = do
            assertOutput (T.pack msg)
            waitFiltering_ EventTy "exited"
      ]

simpleSessions' :: [Int] -> ([FilePath], TestDAPServer) -> [IO ()]
simpleSessions' ls (dirs,server) =
  [ withBreakPoints [ mkBP line     Nothing (Just msg)
                    , mkBP (line+1) Nothing Nothing
                    ] check (d,server)
      | (i,(line,d)) <- zip [(0::Int)..] $ zip ls dirs
      , let msg = "MSG_" ++ show i
      , let
          check = do
            assertOutput (T.pack msg)
            waitFiltering_ EventTy "stopped"
      ]
