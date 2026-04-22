-- | Stack trace tests (issues #107, #159) ported from the NodeJS testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.StackTrace (stackTraceTests) where

import Control.Monad.IO.Class (liftIO)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import DAP (threadId, stackFrameName)

stackTraceTests :: TestTree
stackTraceTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.StackTrace"
    [ testCase "contains mixed IPE and breakpoint frames (issue #107)" mixedFramesTest
    , testCase "displays stack annotation frames (issue #159)" stackAnnotationsTest
    ]

mixedFramesTest :: Assertion
mixedFramesTest =
  withTestDAPServer "test/integration/T107a" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "app/Main.hs"
      hitBreakpointWith cfg 9
      t:_ <- threads
      frames <- stackTrace (threadId t)
      let names = map stackFrameName frames
      liftIO $ do
        -- Contains the interpreter frame
        assertBool ("should contain Main.main.a: " ++ show names)
          ("Main.main.a" `elem` names)
        -- Contains the IPE frame
        assertBool ("should contain MyLib.thenDo: " ++ show names)
          ("MyLib.thenDo" `elem` names)
      disconnect

stackAnnotationsTest :: Assertion
stackAnnotationsTest =
  withTestDAPServer "test/integration/T159" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "app/Main.hs"
      hitBreakpointWith cfg 15
      t:_ <- threads
      frames <- stackTrace (threadId t)
      let names = map stackFrameName frames
      liftIO $ do
        -- Contains the stack annotations
        assertBool ("should contain 'Lovely annotation': " ++ show names)
          ("Lovely annotation" `elem` names)
        assertBool ("should contain '[1,2,3,4]': " ++ show names)
          ("[1,2,3,4]" `elem` names)
      disconnect

