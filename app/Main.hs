{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase, ScopedTypeVariables #-}
{-

== Overview

Main launches 3 lightweight threads.
- One thread listens to requests and writes them to a `Chan` (C1=requests)
- Another reads from `Chan` (C2=replies) and sends replies
- The worker thread reads requests from (C1), executes (e.g. by interpreting the debuggee program until a break), and writes responses to (C2)

┌────────────────────────────────────┐          
│Main                                │          
└┬────────────────────┬─────────────┬┘          
┌▽──────────────────┐┌▽───────────┐┌▽──────────┐
│Listen for requests││Send replies││GHC session│
└───────────────────┘└────────────┘└───────────┘


== Configuration

Currently, there is no support for changing the configuration of `ghc-debugger` at runtime.
- The `ghc-debugger` specific options are passed as process arguments as per 'Settings'.
- The GHC-specific flags that determine how to compile/interpret the project
  are passed after a double dash (@--@) in the list of arguments as well.
  Don't include MODE flags (like --interpreter)!

== Notes

For now, debugging is only supported in the interpreter mode.
When this changes, the code can be revised to accommodate those capabilities.
-}
module Main where

import Data.Functor
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad.Catch

import GHC.IO.Handle
import GHC.IO.Encoding
import System.IO
import System.Exit
import System.Environment (getArgs)

import Debugger
import Debugger.Monad
import Debugger.Interface

-- TODO:Handle errors somewhere here and report them as responses (`prettyPrintGhcErrors`)?

data Settings = Settings
      { --   logLevel?
        -- , force inspect?
        -- , context-modules?
        ghcInvocation :: [String]
      , libdir :: Maybe FilePath
      , units :: [String]
      }

main :: IO ()
main = do
  ghcInvocationFlags <- getArgs

  setLocaleEncoding utf8

  -- Duplicate @stdout@ as @hout@ and move @stdout@ to @stderr@. @hout@ still
  -- points to the standard output, which is now written exclusively by the sender.
  -- This guards against stray prints from corrupting the JSON-RPC message stream.
  hout <- hDuplicate stdout
  stderr `hDuplicateTo` stdout

  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering

  requests <- newChan
  replies  <- newChan

  _ <- forkIO $ receiver requests
  _ <- forkIO $ sender hout replies

  -- Run debugger in main thread
  debugger requests replies
    (mkSettings ghcInvocationFlags)

  return ()

-- | Make 'Settings' from ghc invocation flags
mkSettings :: [String] -> Settings
mkSettings flags = Settings
  { ghcInvocation = flags
  , libdir = listToMaybe $ mapMaybe (\case '-':'B':dir -> Just dir; _ -> Nothing) flags
  , units  = mapMaybe (\case ("-unit", u) -> Just u; _ -> Nothing) $ zip flags (drop 1 flags)
  }

-- | The main worker. Runs a GHC session which executes 'Request's received from
-- the given @'Chan' 'Request'@ and writes 'Response's to the @'Chan' 'Response'@ channel.
debugger :: Chan Request -> Chan Response -> Settings -> IO ()
debugger requests replies Settings{libdir, units, ghcInvocation} =
  runDebugger libdir units ghcInvocation $
    forever $ do
      req <- liftIO $ readChan requests
      resp <- (execute req <&> Right)
                `catch` \(e :: SomeException) -> pure (Left (displayException e))
      either bad reply resp
  where
    reply = liftIO . writeChan replies
    bad   = liftIO . hPutStrLn stderr

execute :: Request -> Debugger Response
execute = \case
  ClearFunctionBreakpoints -> DidClearBreakpoints <$ clearBreakpoints Nothing
  ClearModBreakpoints fp -> DidClearBreakpoints <$ clearBreakpoints (Just fp)
  SetBreakpoint bp -> DidSetBreakpoint <$> setBreakpoint bp BreakpointEnabled
  DelBreakpoint bp -> DidRemoveBreakpoint <$> setBreakpoint bp BreakpointDisabled
  GetStacktrace -> undefined -- decide whether to use a different callstack mechanism or really use :hist?
  GetVariables -> undefined
  GetSource -> undefined
  DoEval exp_s -> DidEval <$> doEval exp_s
  DoContinue -> DidContinue <$> doContinue
  DoSingleStep -> DidStep <$> doSingleStep
  DoStepLocal -> DidStep <$> doLocalStep
  DebugExecution { entryPoint, runArgs } -> DidExec <$> debugExecution entryPoint runArgs
  TerminateProcess -> liftIO $ do
    -- Terminate!
    putStrLn "Goodbye..."
    exitWith ExitSuccess

