-- | 'scopes' request tests
{-# LANGUAGE OverloadedStrings, CPP #-}
module Test.Integration.Scopes (scopesTests) where

import Control.Monad.IO.Class (liftIO)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import DAP (threadId, stackFrameId, scopeName, scopeExpensive)
import Data.List (find)
import qualified Data.Text as T

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
scopesExpensiveTest = withTestDAPServer "test/integration/T44" [] $ \ test_dir server ->

  withTestDAPServerClient server $ do

      _ <- hitBreakpointWith (mkLaunchConfig test_dir "Main.hs") 6

      thread:_ <- threads
      frame:_  <- stackTrace (threadId thread)
      scps     <- scopes (stackFrameId frame)

      let lookupExpensive n = scopeExpensive <$> find ((== T.pack n) . scopeName) scps
      liftIO $ assertEqual "Locals should not be expensive" (Just False) (lookupExpensive "Locals")
      liftIO $ assertEqual "Module should be expensive" (Just True) (lookupExpensive "Module")
      liftIO $ assertEqual "Globals should be expensive" (Just True) (lookupExpensive "Globals")

      disconnect
      pure ()
