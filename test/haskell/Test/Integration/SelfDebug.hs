{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.SelfDebug (selfDebugTests) where

import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Test.DAP
import Test.DAP.Messages.Parser (Event(..))
import Test.Tasty
import Test.Tasty.HUnit
import qualified DAP
import DAP.Types (StoppedEvent(..))
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

selfDebugTests :: TestTree
selfDebugTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.SelfDebug"
    [ testCase "debug the debugger itself via DAP" selfDebugDAPTest
    ]

selfDebugDAPTest :: Assertion
selfDebugDAPTest = do
  withTestDAPServer "." [] $ \test_dir server -> do -- Self-debug test copies project root to temp dir
    doesFileExist (test_dir </> "cabal.project") >>= \exists ->
      unless exists $ writeFile (test_dir </> "cabal.project")
        "packages: . haskell-debugger-view\nallow-newer: ghc-bignum,containers,time,ghc,base,template-haskell"
    doesFileExist (test_dir </> "hie.yaml") >>= \exists ->
      unless exists $ writeFile (test_dir </> "hie.yaml")
        "cradle:\n  cabal:\n    component: \"all\""
    withTestDAPServerClient server $ do
      let cfg = LaunchConfig
            { lcProjectRoot = test_dir
            , lcEntryFile = Just "hdb/Main.hs"
            , lcEntryPoint = Just "main"
            , lcEntryArgs = ["cli", "test/golden/self-debug-cli/Main.hs"]
            , lcExtraGhcArgs = []
            , lcInternalInterpreter = Nothing
            }

      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"

      _ <- sync $ setFunctionBreakpointsRequest @_ @Value $ object
        [ "breakpoints" .= [ object [ "name" .= ("runDebugger" :: String) ] ] ]

      _ <- sync configurationDone

      Event{eventBody = Just StoppedEvent{stoppedEventReason = reason}}
        <- waitFiltering EventTy "stopped"
      liftIO $ assertEqual "expected breakpoint stop"
        reason DAP.StoppedEventReasonBreakpoint
          -- fixme: should we reply with StoppedEventReasonFunctionBreakpoint instead?

      disconnect
