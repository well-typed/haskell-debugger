-- | Multi-threaded debugging tests.
--
-- Checks that, while stopped, the debugger reports exactly the debuggee's own
-- threads (the main thread and the labelled workers) and none of the helper
-- threads used by the interpreter itself.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Integration.Threads (threadsTests) where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import qualified Data.Text as T

import Test.DAP
import Test.Tasty
import Test.Tasty.HUnit
#ifdef mingw32_HOST_OS
import Test.Tasty.ExpectedFailure
#endif
import qualified DAP

threadsTests :: TestTree
threadsTests =
#ifdef mingw32_HOST_OS
  ignoreTestBecause "Needs to be fixed for Windows (#199)" $
#endif
  testGroup "DAP.Integration.Threads"
    [ testCase "only the debuggee threads are listed" onlyDebuggeeThreadsAreListed
    ]

onlyDebuggeeThreadsAreListed :: Assertion
onlyDebuggeeThreadsAreListed =
  withTestDAPServer "test/integration/threads" [] $ \test_dir server ->
    withTestDAPServerClient server $ do

      hitBreakpointWith (mkLaunchConfig test_dir "Main.hs") 25 -- break after spawning the workers

      -- listing all threads should display all the debuggee threads (main + 4 workers)
      -- and no internal helper threads like IOManager or ext interpreter server
      names <- sort . map DAP.threadName <$> threads
      liftIO $ assertEqual "reported threads" expectedThreadNames names

      disconnect
  where
    expectedThreadNames =
      sort ("Debuggee Main" : ["Worker " <> T.pack (show i) | i <- [1 .. 4 :: Int]])
