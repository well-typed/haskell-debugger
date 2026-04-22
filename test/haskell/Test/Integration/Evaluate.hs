-- | Evaluate request tests (issue #116) ported from the NodeJS testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.Evaluate (evaluateTests) where

import Control.Monad.IO.Class (liftIO)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import qualified DAP

evaluateTests :: TestTree
evaluateTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Evaluate"
    [ testCase "Return structured representation for evaluated expressions (issue #116)"
        evaluateStructured
    ]

evaluateStructured :: Assertion
evaluateStructured =
  withTestDAPServer "test/integration/T116" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T116.hs"
      hitBreakpointWith cfg 13

      resp <- evaluate "IM.delete 0 (IM.insert 0 'a' (IM.insert 1 'b' IM.empty))"
      liftIO $ assertEqual "result is IntMap" "IntMap" (DAP.evaluateResponseResult resp)
      respChildren <- fetchChildren (DAP.evaluateResponseVariablesReference resp)
      -- fixme: it'd be good to re-use the VarViews structure here, but that's specialized to Variables for now
      case filter (\v -> DAP.variableName v == "1") respChildren of
        (v1:_) -> v1 @==? "\'b\'"
        []     -> liftIO $ assertFailure $
          "No variable named 1 in evaluation result: " ++ show respChildren
      disconnect
