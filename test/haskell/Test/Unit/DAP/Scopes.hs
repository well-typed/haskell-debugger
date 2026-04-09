-- | 'scopes' request tests
{-# LANGUAGE OverloadedStrings, CPP #-}
module Test.Unit.DAP.Scopes (scopesTests) where

import Control.Monad.IO.Class (liftIO)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Utils (withHermeticDir)

scopesTests :: TestTree
scopesTests =
  testGroup "DAP.Scopes"
    [
#ifdef mingw32_HOST_OS
      ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
      testCase "Locals is cheap, Module/Globals are expensive" scopesExpensiveTest
    ]

scopesExpensiveTest :: Assertion
scopesExpensiveTest = withHermeticDir False "test/unit/T44" $ \test_dir -> do
  server <- startTestDAPServer test_dir ["--disable-ipe-backtraces"]

  withTestDAPServerClient False server $ do

      _ <- defaultHitBreakpoint test_dir 6

      threadId:_ <- threads
      frameId:_ <- stackTrace threadId
      scps <- scopes frameId

      let lookupExpensive n = lookup n scps
      liftIO $ assertEqual "Locals should not be expensive" (Just False) (lookupExpensive "Locals")
      liftIO $ assertEqual "Module should be expensive" (Just True) (lookupExpensive "Module")
      liftIO $ assertEqual "Globals should be expensive" (Just True) (lookupExpensive "Globals")

      disconnect
      pure ()
