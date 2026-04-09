-- | 'logMessage'/'logPoints' tests
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.Unit.DAP.LogMessage (logMessageTests,setupBreakpoints) where

import Control.Monad.IO.Class (liftIO)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils
import Test.DAP.Messages.Parser
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

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
simpleTest tmpl expected =
  logMessageTestSetup [] [(18,Nothing,Just tmpl)] $ do
    assertOutput (T.pack expected)

conditionTest :: IO ()
conditionTest = logMessageTestSetup [] bps $ do
  events <- waitAccumulating Event "output"
  let output = T.concat $ mapMaybe parseOutput events

  liftIO $ assertBool "logged when False" $
    not $ "DO NOT LOG" `T.isInfixOf` output
  liftIO $ assertBool "did not log when True" $
    "DO LOG" `T.isInfixOf` output
  return ()
  where
    bps = [(18,Just "False", Just "DO NOT LOG")
          ,(19,Just "True", Just "DO LOG")
          ]

logMessageTestSetup :: [String] -> [(Int, Maybe String, Maybe String)] -> TestDAP a -> IO ()
logMessageTestSetup flags bps check = do
  withHermeticDir False "test/unit/T113" $ \test_dir -> do

    server <- startTestDAPServer test_dir flags

    withTestDAPServerClient False server $ do
      () <- setupBreakpoints test_dir bps
      _ <- check
      disconnect

-- | Let's you setup multiple breakpoints, with conditions and logs.
--   Any events after configurationDone are left unconsumed.
setupBreakpoints :: FilePath -> [(Int, Maybe String, Maybe String)] -> TestDAP ()
setupBreakpoints testDir bps = do
  _ <- sync $ defaultLaunch testDir
  _ <- waitFiltering Event "initialized"
  _ <- sync $ defaultSetBreakpoints testDir bps
  _ <- sync configurationDone
  pure ()
