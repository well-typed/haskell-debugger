-- | Tests for the @cwd@ and @env@ launch configuration fields.
--
-- See Note [Debuggee working directory and environment] in "GHC.Debugger.Run".
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.CwdEnv (cwdEnvTests) where

import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Data.Text as T
import Test.DAP
import Test.DAP.Messages.Parser (Response(..))
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif

-- | The test program prints its working directory, whether @marker.txt@ exists
-- there, @HDB_TEST_VAR@ and whether @PATH@ starts with @/hdb-stub:@.
testProgram :: FilePath
testProgram = "test/integration/cwd-env"

cwdEnvTests :: TestTree
cwdEnvTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.CwdEnv"
    [ testCase "relative cwd and env" relativeCwdAndEnv
    , testCase "absolute cwd" absoluteCwd
    , testCase "defaults are untouched" defaultsUntouched
    , testCase "reports error when cwd does not exist" missingCwd
    , testCase "rejects cwd with --internal-interpreter" cwdWithInternalInterpreter
    , testCase "env with --internal-interpreter" envWithInternalInterpreter
    , testCase "rejects invalid env variable name" invalidEnvName
    ]

-- | Launch to completion and check the program's output.
runAndCheck :: [String] -> (FilePath -> LaunchConfig -> LaunchConfig) -> [String] -> Assertion
runAndCheck flags mkCfg expectedLines =
  withTestDAPServer testProgram flags $ \test_dir server ->
    withTestDAPServerClient server $ do
      runToEnd (mkCfg test_dir (mkLaunchConfig test_dir "Main.hs"))
      -- Output events are forwarded asynchronously; waitFullOutput waits for them.
      mapM_ (waitFullOutput . T.pack) expectedLines

relativeCwdAndEnv :: Assertion
relativeCwdAndEnv = do
  inheritedPath <- maybe "" id <$> lookupEnv "PATH"
  runAndCheck []
    (\_ cfg -> cfg { lcCwd = Just "sub"
                   , lcEnv = Just [ ("HDB_TEST_VAR", "from-launch")
                                  -- What the editor sends after substituting ${env:PATH}
                                  , ("PATH", "/hdb-stub:" ++ inheritedPath) ] })
    [ "cwd: sub", "marker: True", "HDB_TEST_VAR: Just \"from-launch\"", "PATH stub first: True" ]

absoluteCwd :: Assertion
absoluteCwd =
  runAndCheck []
    (\test_dir cfg -> cfg { lcCwd = Just (test_dir </> "sub") })
    [ "cwd: sub", "marker: True", "HDB_TEST_VAR: Nothing" ]

defaultsUntouched :: Assertion
defaultsUntouched =
  runAndCheck [] (\_ cfg -> cfg)
    [ "marker: False", "HDB_TEST_VAR: Nothing", "PATH stub first: False" ]

envWithInternalInterpreter :: Assertion
envWithInternalInterpreter =
  runAndCheck ["--internal-interpreter"]
    (\_ cfg -> cfg { lcEnv = Just [("HDB_TEST_VAR", "internal")] })
    [ "HDB_TEST_VAR: Just \"internal\"" ]

-- | Launch with a config that should fail, asserting on the error message.
launchShouldFail :: [String] -> (LaunchConfig -> LaunchConfig) -> String -> Assertion
launchShouldFail flags mkCfg expectedPrefix =
  withTestDAPServer testProgram flags $ \test_dir server ->
    withTestDAPServerClientWith False (\msg val -> do
        assertBool ("Expected launch error starting with " ++ show expectedPrefix ++ ", got: " ++ show msg)
          (expectedPrefix `isPrefixOf` msg)
        pure (Just val) -- continue with success=False...
      ) server $ do
      Response{responseSuccess} <- sync $ launchWith (mkCfg (mkLaunchConfig test_dir "Main.hs"))
      liftIO $ assertBool "Expected launch to fail, but got success: true" (not responseSuccess)

missingCwd :: Assertion
missingCwd =
  launchShouldFail [] (\cfg -> cfg { lcCwd = Just "does-not-exist" })
    "\"cwd\" directory does not exist"

cwdWithInternalInterpreter :: Assertion
cwdWithInternalInterpreter =
  launchShouldFail ["--internal-interpreter"] (\cfg -> cfg { lcCwd = Just "sub" })
    "\"cwd\" is not supported when hdb runs with --internal-interpreter"

invalidEnvName :: Assertion
invalidEnvName =
  launchShouldFail [] (\cfg -> cfg { lcEnv = Just [("A=B", "x")] })
    "Invalid \"env\" variable name"
