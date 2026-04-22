-- | Multiple home unit tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.MultiHomeUnit (multiHomeUnitTests) where

import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

multiHomeUnitTests :: TestTree
multiHomeUnitTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.MultiHomeUnit"
    [ testCase "should run program to the end" runToTheEnd
    , testCase "should stop at break-point in the same home unit" sameHomeUnitBP
    , testCase "should stop at break-point in different home unit 1" otherHomeUnitBP1
    , testCase "should stop at break-point in different home unit 2" otherHomeUnitBP2
    ]

withCommonSetup :: (FilePath -> TestDAP ()) -> Assertion
withCommonSetup test =
  withTestDAPServer "test/integration/cabal-mhu1" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      test test_dir
      disconnect

runToTheEnd :: Assertion
runToTheEnd = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  runToEnd cfg

sameHomeUnitBP :: Assertion
sameHomeUnitBP = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  hitBreakpointWith cfg 8

otherHomeUnitBP1 :: Assertion
otherHomeUnitBP1 = withCommonSetup $ \test_dir -> do
  -- Use bar/app/Main.hs as the entry file; set a breakpoint in a
  -- *different* home unit (bar/src/Bar.hs).
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  hitBreakpointIn cfg "bar/src/Bar.hs" 8

otherHomeUnitBP2 :: Assertion
otherHomeUnitBP2 = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  hitBreakpointIn cfg "foo/src/Foo.hs" 6
