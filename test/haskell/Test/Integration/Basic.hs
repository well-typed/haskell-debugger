-- | Basic launch/run tests ported from the old NodeJS integration testsuite.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Integration.Basic (basicTests) where

import Control.Monad.Reader
import Test.DAP
import Test.DAP.Messages.Parser
import Test.Tasty
import Test.Tasty.HUnit
import qualified DAP

basicTests :: TestTree
basicTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Basic"
    [ testGroup "Most basic functionality"
        [ basicForConfig "Vanilla config (no package)" "test/integration/simple" "Main.hs"
        , basicForConfig "Cabal config" "test/integration/cabal1" "app/Main.hs"
        , testGroup "Other basic tests"
            [ testCase "report error on missing entryFile" reportMissingEntryFile
            , testCase "minimal configuration with just entryFile" minimalConfig
            , testCase "accepts internalInterpreter launch option" internalInterpreterOption
            ]
        ]
    ]

basicForConfig :: TestName -> FilePath -> FilePath -> TestTree
basicForConfig name projectRoot entryFile =
  testGroup name
    [ testSetup "should run program to the end" $ \cfg -> do
        runToEnd cfg
    , testSetup "should stop on a breakpoint" $ \cfg -> do
        hitBreakpointWith cfg 6
        disconnect
    , testSetup "should stop on an exception" $ \cfg -> do
        _ <- sync $ launchWith cfg
        waitFiltering_ EventTy "initialized"
        setBreakOnException
        _ <- sync configurationDone
        assertStoppedLocation DAP.StoppedEventReasonException 66 -- FIXME WHY IS THIS 66? in the NodeJS testsuite we ignored the line.
        disconnect
    ]
  where
    testSetup s k = testCase s $
      withTestDAPServer projectRoot [] $ \test_dir server ->
        withTestDAPServerClient server $ do
          let cfg = (mkLaunchConfig test_dir entryFile) { lcEntryArgs = ["some", "args"] }
          k cfg

reportMissingEntryFile :: Assertion
reportMissingEntryFile =
  withTestDAPServer "test/integration/T71" [] $ \test_dir server ->
    withTestDAPServerClientWith False (\msg val -> do
        assertBool "test should fail with missing key"
          (msg == "Missing \"entryFile\" key in debugger configuration")
        pure (Just val) -- continue with success=False...
      ) server $ do
      let cfg = (mkLaunchConfig test_dir "Main.hs") { lcEntryFile = Nothing }
      Response{responseSuccess} <- sync $ launchWith cfg
      liftIO $ assertBool "Expected test to fail with no entry file, but got success: true"
        (not responseSuccess)


minimalConfig :: Assertion
minimalConfig =
  withTestDAPServer "test/integration/T71" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = LaunchConfig
            { lcProjectRoot = test_dir
            , lcEntryFile = Just "Main.hs"
            , lcEntryPoint = Nothing
            , lcEntryArgs = []
            , lcExtraGhcArgs = []
            , lcInternalInterpreter = Nothing
            }
      hitBreakpointWith cfg 2
      disconnect

internalInterpreterOption :: Assertion
internalInterpreterOption =
  withTestDAPServer "test/integration/simple" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = (mkLaunchConfig test_dir "Main.hs")
            { lcInternalInterpreter = Just True } -- TODO: Automatically run all tests with internal interpreter too?
      hitBreakpointWith cfg 6
      disconnect
