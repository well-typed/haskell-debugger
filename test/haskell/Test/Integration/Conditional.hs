-- | Conditional breakpoint tests (issue #111) ported from the NodeJS testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.Conditional (conditionalTests) where

import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import qualified DAP
import DAP (SourceBreakpoint(..), defaultSourceBreakpoint)

conditionalTests :: TestTree
conditionalTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Conditional"
    [ testCase "Conditional expression breakpoints" conditionalExpr
    , testCase "Hit count breakpoints" hitCount
    ]

conditionalExpr :: Assertion
conditionalExpr =
  testWith False
    defaultSourceBreakpoint
      { sourceBreakpointLine = 13
      , sourceBreakpointCondition = Just "im IM.! 0 == 2" }
    -- Continuing just once is sufficient for it to exit now
    (continueThread 0)

hitCount :: Assertion
hitCount =
  testWith True
    defaultSourceBreakpoint
      { sourceBreakpointLine = 13
      , sourceBreakpointHitCondition = Just "2"
        -- this ^ ignores 2 hits and stops at third.
      }
    -- Unlike conditional expression, we hit the breakpoint every time
    -- after the ignore count, so run this twice
    (continueThread 0 >> continueThread 0)

testWith :: Bool -> SourceBreakpoint -> TestDAP () -> Assertion
testWith needsForcing bp afterCheck =
  withTestDAPServer "test/integration/T111" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T111.hs"

      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      _ <- sync $ setBreakpointsIn test_dir "T111.hs" [bp]
      _ <- sync configurationDone
      assertStoppedLocation DAP.StoppedEventReasonBreakpoint 13

      -- This assertion validates:
      --  1) we only stopped when im IM.! 0 == 2 (and not earlier)
      --  2) or we only stopped on the third iteration
      -- This test is run with two different breakpoints to trigger the two
      -- cases, but both are validated the same way.
      vars <- fetchLocalVars
      -- In the expression conditional case, we have already forced IM to evaluate the condition
      -- In the hit conditional case, we haven't evaluated IM so we need to.
      im <- (if needsForcing then forceLazy else pure) (vars % "im")

      im @==? "IntMap"
      imChild <- expandVar im
      let _2v = imChild % "0"
          _3v = imChild % "2"
      _2v @==? "2"
      _3v @==? "4"
      afterCheck
      waitForExitCode 0
      disconnect
