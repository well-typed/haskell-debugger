-- | 'logMessage'/'logPoints' tests
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Test.DAP.LogMessage (logMessageTests) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.List (isInfixOf)
import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import Test.Utils

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
        -- this fails to compile the log message expression, I guess evaluation context for `doEval` doesn't inherit module LANGUAGE pragmas?
        -- , testCase "overloaded list" $ logMessageSimpleTest
        --   "{ show $ ([1,2,3] :: R) }" "R {field = 1}"
        , testCase "log only when condition" $
          conditionTest
        ]
    ]

simpleTest :: String -> String -> IO ()
simpleTest tmpl expected =
  logMessageTestSetup "" [(18,Nothing,Just tmpl)] $ do
    vs <- receiveMessagesUnordered [eventMatch "output"]
    hasLogMsg expected vs

conditionTest :: IO ()
conditionTest = logMessageTestSetup "" bps $ do
  -- conditions are printed when they are evaluated.
  expectMessagesUnordered $ replicate 2 $ eventMatch "output"
  [v] <- receiveMessagesUnordered [eventMatch "output"]
  Just output <- pure $ getOutput v
  liftIO $ assertBool "logged when False" $
    not $ "DO NOT LOG" `isInfixOf` output
  liftIO $ assertBool "did not log when True" $
    "DO LOG" `isInfixOf` output
  return ()
  where
    bps = [(18,Just "False", Just "DO NOT LOG")
          ,(19,Just "True", Just "DO LOG")
          ]

logMessageTestSetup :: String -> [(Int, Maybe String, Maybe String)] -> TestDAP a -> IO ()
logMessageTestSetup flags bps check = do
  withHermeticDir False "test/unit/T113" $ \test_dir -> do

    server <- startTestDAPServer test_dir flags

    withTestDAPServerClient server $ do
      () <- setupBreakpoints test_dir $ bps
      _ <- check

      -- consume further prints and events
      expectMessagesUnordered $
        replicate 3 (eventMatch "output")
          ++ [eventMatch "terminated", eventMatch "exited"]

      -- Send disconnect
      disconnectSession
      pure ()

-- | Let's you setup multiple breakpoints, with conditions and logs.
--   Any events after configurationDone are left unconsumed.
setupBreakpoints :: FilePath -> [(Int, Maybe String, Maybe String)] -> TestDAP ()
setupBreakpoints testDir bps = do
  sendInitialize False
  expectMessagesUnordered [responseMatch "initialize"]

  sendLaunch testDir
  expectMessagesUnordered $
    replicate 9 (eventMatch "output")
      ++ [ responseMatch "launch"
         , eventMatch "initialized"
         ]

  sendSetBreakpoints' testDir bps
  expectMessagesUnordered [responseMatch "setBreakpoints"]

  sendConfigurationDone
  expectMessagesUnordered
    [ responseMatch "configurationDone"
    ]

  pure ()

getOutput :: Value -> Maybe String
getOutput = parseMaybe $ withObject "event" $ \ o -> do
    body <- o .: "body"
    String output <- body .: "output"
    pure $ T.unpack output

hasLogMsg :: String -> [Value] -> TestDAP ()
hasLogMsg expected vs = liftIO $ do
  let
    p v = do
      output <- getOutput v
      pure $ output == expected ++ "\n"
  assertBool ("missing expected log msg: " ++ expected) $ any (fromMaybe False . p) vs
