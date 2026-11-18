-- | 'logMessage'/'logPoints' tests
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Test.Integration.LogMessage (logMessageTests) where

import Control.Monad.IO.Class (liftIO)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import DAP (SourceBreakpoint(..), defaultSourceBreakpoint)

logMessageTests :: TestTree
logMessageTests =
#ifdef mingw32_HOST_OS
 ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.LogMessage"
    [
      testGroup "breakpoint with logMessage"
        [ testCase "fixed message" $
          simpleTest
            "unique log message string"
            "unique log message string"
        , testCase "interpolation" $
          simpleTest
            "f lcl = {f lcl}"
            "f lcl = f local"
        , testCase "escape in message" $
          simpleTest "\\{ }" "{ }"
        , testCase "escape in antiquote" $
          simpleTest "{ show $ R { field = 1 \\} }"
            "R {field = 1}"
        , testCase "overloaded list" $ simpleTest
          "{ show $ ([1,2,3] :: R) }" "R {field = 1}"
        , testCase "log only when condition" $
          conditionTest
        ]
    ]

simpleTest :: String -> String -> IO ()
simpleTest tmpl expected = do
  let bp = defaultSourceBreakpoint
        { sourceBreakpointLine = 18
        , sourceBreakpointLogMessage = Just (T.pack tmpl) }
  logMessageTestSetup [] [bp] $ do
    output <- programOutput
    liftIO $ assertBool ("expecting " ++ show expected ++ " but got " ++ show output) $
      T.pack expected `T.isInfixOf` output

conditionTest :: IO ()
conditionTest = logMessageTestSetup [] bps $ do
  output <- programOutput

  liftIO $ assertBool "logged when False" $
    not $ "DO NOT LOG" `T.isInfixOf` output
  liftIO $ assertBool "did not log when True" $
    "DO LOG" `T.isInfixOf` output
  return ()
  where
    bps =
      [ defaultSourceBreakpoint
          { sourceBreakpointLine = 18
          , sourceBreakpointCondition = Just "False"
          , sourceBreakpointLogMessage = Just "DO NOT LOG" }
      , defaultSourceBreakpoint
          { sourceBreakpointLine = 19
          , sourceBreakpointCondition = Just "True"
          , sourceBreakpointLogMessage = Just "DO LOG" }
      ]

-- | The output up to the line that T113 prints last.
programOutput :: TestDAP T.Text
programOutput = waitForOutput "goodbye"

logMessageTestSetup :: [String] -> [SourceBreakpoint] -> TestDAP a -> IO ()
logMessageTestSetup flags bps check =
  withTestDAPServer "test/integration/T113" flags $ \test_dir server ->
    withTestDAPServerClient server $ do
      _ <- sync $ launchWith (mkLaunchConfig test_dir "Main.hs")
      waitFiltering_ EventTy "initialized"
      _ <- sync $ setBreakpointsIn test_dir "Main.hs" bps
      _ <- sync configurationDone
      _ <- check
      disconnect
