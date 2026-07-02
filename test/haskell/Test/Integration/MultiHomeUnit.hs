-- | Multiple home unit tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.MultiHomeUnit (multiHomeUnitTests) where

import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((</>))
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

multiHomeUnitTests :: TestTree
multiHomeUnitTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.MultiHomeUnit"
    [testGroup dirname [ testCase "should run program to the end" $ runToTheEnd dirname
    , testCase "should stop at break-point in the same home unit" $ sameHomeUnitBP dirname
    , testCase "should stop at break-point in different home unit 1" $ otherHomeUnitBP1 dirname
    , testCase "should stop at break-point in different home unit 2" $ otherHomeUnitBP2 dirname
    ] |
    dirname <- ["cabal-mhu1","T38"]
    ]
withCommonSetup :: (FilePath -> TestDAP ()) -> String -> Assertion
withCommonSetup test dirname =
  withTestDAPServer ("test/integration" </> dirname) [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      test test_dir
      disconnect

runToTheEnd :: String -> Assertion
runToTheEnd = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  runToEnd cfg

sameHomeUnitBP :: String -> Assertion
sameHomeUnitBP = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  hitBreakpointWith cfg 8

otherHomeUnitBP1 :: String -> Assertion
otherHomeUnitBP1 = withCommonSetup $ \test_dir -> do
  -- Use bar/app/Main.hs as the entry file; set a breakpoint in a
  -- *different* home unit (bar/src/Bar.hs).
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  hitBreakpointIn cfg "bar/src/Bar.hs" 8

otherHomeUnitBP2 :: String -> Assertion
otherHomeUnitBP2 = withCommonSetup $ \test_dir -> do
  let cfg = mkLaunchConfig test_dir "bar/app/Main.hs"
  hitBreakpointIn cfg "foo/src/Foo.hs" 6
