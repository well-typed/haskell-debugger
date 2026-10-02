-- | Basic launch/run tests ported from the old NodeJS integration testsuite.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Integration.Basic (basicTests) where

import qualified Control.Monad.Catch as MC
import Control.Monad.Reader
import qualified Data.List as List
import Data.Aeson (Value)
import Data.Either (isLeft)
import System.FilePath
import System.Timeout
import Test.DAP
import Test.DAP.Messages.Parser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure (expectFailBecause)
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
    , testGroup "Multi-module standalone (no cabal/hie.yaml)"
        [ testCase "breakpoints in two modules (#297)" multiModuleStandaloneBreakpoints
        , testCase "breakpoints in two modules (flipped) (#297)" multiModuleStandaloneBreakpoints2
        ]
    , testGroup "Ending session"
        [ expectFailBecause "DAP thread stuck waiting for EvalResult" $
           testGroup "debuggee idle"
             [ testCase "disconnect promptly" debuggeeIdleDisconnectTest
             , testCase "terminate promptly" debuggeeIdleTerminateTest
             ]
        , testCase "debuggee idle testcase loads" debuggeeIdleTestSetupTest
        ]
    , testCase "report error when projectRoot does not exist" reportErrorForMissingDirectory
    , testCase "throws exception when no more messages" throwsExceptionWhenNoMoreMessages
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
        assertStoppedLocation DAP.StoppedEventReasonException 10 -- Line in the test file
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
            , lcCradleFile = Nothing
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

-- | Two-module standalone project (no cabal, no hie.yaml): set a breakpoint in
-- each module, run, hit the first (Main.hs), continue, hit the second (Helper.hs).
-- (#297)
multiModuleStandaloneBreakpoints :: Assertion
multiModuleStandaloneBreakpoints =
  withTestDAPServer "test/integration/standalone-multi-module" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      _ <- sync $ setLineBreakpoints test_dir "Main.hs"   [7]
      _ <- sync $ setLineBreakpoints test_dir "Helper.hs" [5]
      _ <- sync configurationDone
      assertStoppedLocation DAP.StoppedEventReasonBreakpoint 7
      continueThread =<< getCurrentActiveThread
      assertStoppedLocation DAP.StoppedEventReasonBreakpoint 5
      disconnect

-- | Same as above, but flip the order of the setLineBreakpoints calls (#297)
multiModuleStandaloneBreakpoints2 :: Assertion
multiModuleStandaloneBreakpoints2 =
  withTestDAPServer "test/integration/standalone-multi-module" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      _ <- sync $ setLineBreakpoints test_dir "Helper.hs" [5]
      _ <- sync $ setLineBreakpoints test_dir "Main.hs"   [7]
      _ <- sync configurationDone
      assertStoppedLocation DAP.StoppedEventReasonBreakpoint 7
      continueThread =<< getCurrentActiveThread
      assertStoppedLocation DAP.StoppedEventReasonBreakpoint 5
      disconnect

debuggeeIdleDisconnectTest :: IO ()
debuggeeIdleDisconnectTest = debuggeeIdleTestSetup $ do
  disconnect
  waitFiltering_ EventTy "terminated"
  assertFullOutput "debuggee shutting down"

debuggeeIdleTerminateTest :: IO ()
debuggeeIdleTerminateTest = debuggeeIdleTestSetup $ do
  terminate
  waitFiltering_ EventTy "terminated"
  assertFullOutput "debuggee shutting down"

debuggeeIdleTestSetupTest :: IO ()
debuggeeIdleTestSetupTest = debuggeeIdleTestSetup $ do
  (_ :: Value) <- waitFiltering' EventTy (stdoutMatch "Started\n")
  pure ()

debuggeeIdleTestSetup :: TestDAP () -> IO ()
debuggeeIdleTestSetup test = do
  let projectRoot = "test/integration/T325a"
  let entryFile = "T325a.hs"
  withTestDAPServer projectRoot [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir entryFile
      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      _ <- sync configurationDone
      withTimeout test
  where
    -- we do our own timeout check as it's part of the spec and plays better
    -- with withTestDAPServerClient
    withTimeout (TestDAP m) = TestDAP $ \ env -> do
      x <- timeout 5_000_000 $ m env
      case x of
        Just a  -> pure a
        Nothing -> assertFailure "Timeout after 5s"

reportErrorForMissingDirectory :: IO ()
reportErrorForMissingDirectory = do
  withTestDAPServer "test/integration/T44" [] $ \test_dir server ->
    withTestDAPServerClientWith False checkErrorResponse server $ do
      let cfg = mkLaunchConfig (test_dir </> "missing") "Main.hs"
      Response{responseSuccess} <- sync $ launchWith cfg
      liftIO $ assertBool
        "Expected launch to fail because of a not existing directory, but got success: true"
        (not responseSuccess)

  where
    checkErrorResponse errMsg v = do
      assertBool ("unexpected error msg: " ++ errMsg)
               ("Couldn't execute ghc --numeric-version" `List.isInfixOf` errMsg)
      return (Just v)

throwsExceptionWhenNoMoreMessages :: IO ()
throwsExceptionWhenNoMoreMessages = do
  withTestDAPServer "test/integration/T44" [] $ \_test_dir server ->
    withTestDAPServerClient server $ do
      disconnect
      e <- MC.try @_ @TestDAPClientConnectionClosed $ waitFiltering_ EventTy "stopped"
      liftIO $ assertBool "got () instead of exception" $ isLeft e
