-- | Evaluate request tests ported from the NodeJS testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.Evaluate (evaluateTests) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import qualified DAP

evaluateTests :: TestTree
evaluateTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Evaluate"
    [ testCase "Return structured representation for evaluated expressions (issue #116)"
        evaluateStructured
    , testCase "Imported module bindings available in evaluate context (issue #233)"
        evaluateImportedBindings
    , expectFailBecause "#299" $
        testCase "Bindings from other modules not available in evaluate context (issue #233)"
          evaluateImportedBindingsNotInOtherModule
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

-- | Test that bindings from imported modules are available when evaluating
-- expressions at a breakpoint (issue #233).
evaluateImportedBindings :: Assertion
evaluateImportedBindings =
  withTestDAPServer "test/integration/T233" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T233.hs"
      hitBreakpointWith cfg 15

      -- sort is imported from Data.List
      sortResp <- evaluate "show (sort xs)"
      liftIO $ assertEqual "sort xs result" "\"[1,1,2,3,4,5,6,9]\"" (DAP.evaluateResponseResult sortResp)

      -- Map is a qualified import
      mapResp <- evaluate "show (Map.lookup \"a\" m)"
      liftIO $ assertEqual "Map.lookup result" "\"Just 1\"" (DAP.evaluateResponseResult mapResp)

      disconnect

-- | Test that bindings from modules not imported at the stopped location are
-- NOT available in the evaluate context (issue #233).
evaluateImportedBindingsNotInOtherModule :: Assertion
evaluateImportedBindingsNotInOtherModule =
  withTestDAPServer "test/integration/T233" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T233.hs"

      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      -- T233.hs imports Data.Map.Strict as Map; Other.hs does not
      _ <- sync $ setLineBreakpoints test_dir "T233.hs" [15]
      _ <- sync $ setLineBreakpoints test_dir "Other.hs" [4]
      _ <- sync configurationDone
      _ <- assertStoppedLocation DAP.StoppedEventReasonBreakpoint 15

      -- Stopped in T233.hs which imports Data.List and Data.Map.Strict as Map
      sortResp <- evaluate "show (sort xs)"
      liftIO $ assertEqual "sort xs result" "\"[1,1,2,3,4,5,6,9]\"" (DAP.evaluateResponseResult sortResp)

      -- Resume and stop at breakpoint in Other.hs, which does not import Map
      continueThread 0
      _ <- assertStoppedLocation DAP.StoppedEventReasonBreakpoint 4

      -- Map is not imported in Other.hs; evaluating Map.fromList should fail
      mapFailResp <- evaluate "Map.fromList [(1,'a')]"
      let result = DAP.evaluateResponseResult mapFailResp
      liftIO $ assertBool
        ("expected 'not in scope' error for Map.fromList, got: " ++ show result)
        ("not in scope" `isInfixOf` show result)

      disconnect
