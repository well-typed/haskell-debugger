-- | Multi-main tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.MultiMain (multiMainTests) where

import qualified Data.Text as T
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

multiMainTests :: TestTree
multiMainTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.MultiMain"
    [ testCase "should run program to the end" runToTheEnd
    , testCase "should stop at break-point" stopAtBreakpoint
    , testCase "should run the right main" runRightMain
    ]

withCommonSetup :: (FilePath -> TestDAP ()) -> Assertion
withCommonSetup test =
  withTestDAPServer "test/integration/multi-mains" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      test test_dir
      disconnect

runToTheEnd :: Assertion
runToTheEnd = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "app/Main.hs"
  runToEnd cfg

stopAtBreakpoint :: Assertion
stopAtBreakpoint = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "app/Main.hs"
  hitBreakpointWith cfg 6

runRightMain :: Assertion
runRightMain = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "second/Main.hs"
  runToEnd cfg
  assertFullOutput (T.pack "The right main")
  assertNotFullOutput (T.pack "The bad main")
