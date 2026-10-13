-- | Evaluate request tests ported from the NodeJS testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Integration.Evaluate (evaluateTests) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf, sort)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import qualified DAP
import DAP.Types
import Control.Monad
import Data.Maybe
import Test.Tasty.ExpectedFailure
import Data.Text (Text)

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
    , testCase "Bindings from other modules not available in evaluate context (issue #233)"
        evaluateImportedBindingsNotInOtherModule
    ,
#ifndef GHC_HAS_MULTITHREADED_DBG
      ignoreTestBecause "Multiple stopped threads not fully supported" $
#endif
        testCase "Evaluate from multiple stopped threads"
          evaluateMultiThreaded
    , testGroup "scope from earlier stack frames (issue #370)"
      [ testCase "multiple stack frames" stackFramesTest
      , testCase "RetBCO waiting for unboxed tuple" stackFramesUnboxedTupleTest
      ]
    ]

evaluateMultiThreaded :: Assertion
evaluateMultiThreaded =
  withTestDAPServer "test/integration/T370" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg@LaunchConfig{..} = mkLaunchConfig test_dir "T370.hs"
      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      _ <- sync $ setLineBreakpoints lcProjectRoot (fromMaybe undefined lcEntryFile) [22,28]
      _ <- sync configurationDone
      -- TODO: explicitly wait for all threads to stop.
      waitFiltering_ EventTy "stopped"
      ts <- Test.DAP.threads
      resps <- fmap catMaybes . forM ts $ \ t -> do
        frs <- stackTrace (threadId t)
        case frs of
          []     -> pure Nothing
          (fr:_) -> Just <$> evaluateAt "x" (Just (stackFrameId fr))
      let results = sort $ map DAP.evaluateResponseResult resps
      liftIO $ assertEqual ("Results: " ++ show results) ["0","2"] results
      disconnect

stackFramesTest :: Assertion
stackFramesTest = stackFramesTestGen "test/integration/T160"

stackFramesTestGen :: FilePath -> Assertion
stackFramesTestGen path =
  withTestDAPServer path [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointIn cfg "X.hs" 22
      frames <- stackTrace =<< getCurrentActiveThread

      res1 <- evaluateAt "head xs" (Just $ stackFrameId (frames !! 1)) 
      assertEqualResult res1 "1"

      res2 <- evaluateAt "s" (Just $ stackFrameId (frames !! 2)) 
      assertEqualResult res2 "\"hello\""

      res3 <- evaluateAt "x" (Just $ stackFrameId (frames !! 3)) 
      assertEqualResult res3 "5"

      disconnect

assertEqualResult :: MonadIO m => EvaluateResponse -> Text -> m ()
assertEqualResult res x = liftIO $ assertEqual (show res) (DAP.evaluateResponseResult res) x

stackFramesUnboxedTupleTest :: Assertion
stackFramesUnboxedTupleTest =
  withTestDAPServer "test/integration/T160" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      hitBreakpointIn cfg "X.hs" 27
      frames <- stackTrace =<< getCurrentActiveThread

      res <- evaluateAt "x" (Just $ stackFrameId (frames !! 1))
      assertEqualResult res "1"

      disconnect

evaluateStructured :: Assertion
evaluateStructured =
  withTestDAPServer "test/integration/T116" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "T116.hs"
      hitBreakpointWith cfg 13

      resp <- evaluateAtBreak "IM.delete 0 (IM.insert 0 'a' (IM.insert 1 'b' IM.empty))"
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
      sortResp <- evaluateAtBreak "show (sort xs)"
      liftIO $ assertEqual "sort xs result" "\"[1,1,2,3,4,5,6,9]\"" (DAP.evaluateResponseResult sortResp)

      -- Map is a qualified import
      mapResp <- evaluateAtBreak "show (Map.lookup \"a\" m)"
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
      sortResp <- evaluateAtBreak "show (sort xs)"
      liftIO $ assertEqual "sort xs result" "\"[1,1,2,3,4,5,6,9]\"" (DAP.evaluateResponseResult sortResp)

      -- Resume and stop at breakpoint in Other.hs, which does not import Map
      continueThread =<< getCurrentActiveThread
      _ <- assertStoppedLocation DAP.StoppedEventReasonBreakpoint 4

      -- Map is not imported in Other.hs; evaluating Map.fromList should fail
      mapFailResp <- evaluateAtBreak "Map.fromList [(1,'a')]"
      let result = DAP.evaluateResponseResult mapFailResp
      liftIO $ assertBool
        ("expected 'not in scope' error for Map.fromList, got: " ++ show result)
        (or [not_in_scope `isInfixOf` show result
            | not_in_scope <- ["not in scope","Not in scope"]])

      disconnect
