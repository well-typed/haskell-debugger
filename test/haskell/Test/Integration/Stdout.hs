-- | Stdout/stderr tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.Stdout (stdoutTests) where

import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

stdoutTests :: TestTree
stdoutTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Stdout"
    [ testCase "basic output and stderr" basicOutputTest
    ]

basicOutputTest :: Assertion
basicOutputTest =
  withTestDAPServer "test/integration/T45" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      -- Hit breakpoint at line 3 before program runs through.
      hitBreakpointWith cfg 3
      next -- Print line
      next -- Reach crash line
      next -- Crash it
      waitFiltering_ EventTy "terminated"

      -- Assert both stdout and stderr appeared somewhere in the accumulated
      -- full output.
      assertFullOutput "Going to read args"
      assertFullOutput "Uncaught exception"
      disconnect
