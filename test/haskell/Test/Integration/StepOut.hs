-- | Step-out tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.StepOut (stepOutTests) where

import DAP (StoppedEventReason(..))
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

-- Currently we depend on this to work around the fact that '>>=' is in library
-- code because @base@ is not being interpreted.
need_opt :: Bool
need_opt = True

stepOutTests :: TestTree
stepOutTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.StepOut"
    [ testCase "simple step-out to case" simpleStepOutCase
    , testCase "without tail calls (T26042b)" withoutTailCalls
    , testCase "with tail calls (T26042c)" withTailCalls
    ]

optFlags :: [String]
optFlags = if need_opt then ["-O", "-fno-unoptimized-core-for-interpreter"] else []

withCommon :: String -> Int -> [String] -> TestDAP () -> Assertion
withCommon entry line flags act =
  withTestDAPServer "test/integration/T6" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = (mkLaunchConfig test_dir entry) { lcExtraGhcArgs = flags }
      hitBreakpointWith cfg line
      act

simpleStepOutCase :: Assertion
simpleStepOutCase = withCommon "MainC.hs" 9 [] $ do
  stepOut 0
  assertStoppedLocation StoppedEventReasonStep 5
  disconnect

withoutTailCalls :: Assertion
withoutTailCalls = withCommon "MainA.hs" 10 optFlags $ do
  -- foo to bar
  stepOut 0
  assertStoppedLocation StoppedEventReasonStep 20
  stepOut 0
  -- bar back to foo
  assertStoppedLocation StoppedEventReasonStep 14
  stepOut 0
  -- back to main
  assertStoppedLocation StoppedEventReasonStep 5
  -- exit
  stepOut 0
  disconnect

-- Mimics GHC's T26042c
withTailCalls :: Assertion
withTailCalls = withCommon "MainB.hs" 10 optFlags $ do
  -- step out of foo True and observe that we have skipped its call in bar,
  -- and the call of bar in foo False.
  -- we go straight to `main`.
  stepOut 0
  assertStoppedLocation StoppedEventReasonStep 5
  -- stepping out again exits
  stepOut 0
  disconnect
