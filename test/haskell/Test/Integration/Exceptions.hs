-- | Exception info tests ported from the NodeJS integration testsuite.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Integration.Exceptions (exceptionTests) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Test.DAP
import Test.DAP.Messages.Parser (Event(..))
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import qualified DAP

exceptionTests :: TestTree
exceptionTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Exceptions"
    [ testCase "reports nested exceptions and continues after the first break" nestedExceptions
    ]

-- Note: It is not clear that stopping 5 times is the right thing to happen
-- here, but it just tests the existing behaviour.
nestedExceptions :: Assertion
nestedExceptions =
  withTestDAPServer "test/integration/exceptions" [] $ \test_dir server ->
    withTestDAPServerClient server $ do
      let cfg = mkLaunchConfig test_dir "Main.hs"
      _ <- sync $ launchWith cfg
      waitFiltering_ EventTy "initialized"
      setBreakOnException
      _ <- sync configurationDone
      checkNested
      disconnect
  where
    assertExcMessage :: String -> T.Text -> (DAP.ExceptionDetails -> IO ()) -> TestDAP ()
    assertExcMessage descr expected k = do
      -- Read next "stopped" event and its thread id
      Event{eventBody = Just DAP.StoppedEvent{DAP.stoppedEventReason, DAP.stoppedEventThreadId}}
        <- waitFiltering EventTy "stopped"
      liftIO $ assertEqual (descr ++ ": should be an exception") DAP.StoppedEventReasonException stoppedEventReason
      let tid = maybe (error "no tid?") id stoppedEventThreadId
      info <- exceptionInfo tid
      let details = DAP.exceptionInfoResponseDetails info
      liftIO $ case details of
        Nothing -> assertFailure $ descr ++ ": exception details should be present"
        Just d  -> do
          assertEqual (descr ++ ": message")
            (Just (T.unpack expected)) (DAP.exceptionDetailsMessage d)
          k d
      continueThread tid

    checkNested :: TestDAP ()
    checkNested = do
      assertExcMessage "first exception" "outer boom" $ \details -> do
        case DAP.exceptionDetailsInnerException details of
          Nothing -> pure ()
          Just [] -> pure ()
          Just _  -> assertFailure "First exception should not have inner exceptions"
      assertExcMessage "second exception" "inner boom" $ const $ pure ()
      assertExcMessage "third exception" "inner boom"  $ const $ pure ()
      assertExcMessage "fourth exception" "inner boom" $ const $ pure ()
      assertExcMessage "fifth exception" "inner boom"  $ const $ pure ()
      waitFiltering_ EventTy "terminated"
