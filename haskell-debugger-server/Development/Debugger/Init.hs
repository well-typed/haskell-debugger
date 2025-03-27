{-# LANGUAGE OverloadedStrings, RecordWildCards, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}

-- | TODO: This module should be called Launch.
module Development.Debugger.Init where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Function
import Data.Functor
import Control.Monad.IO.Class
import System.IO
import Control.Monad.Catch
import Control.Exception (someExceptionContext)
import Control.Exception.Context
import Control.Concurrent
import Control.Monad
import Data.Aeson as Aeson
import GHC.Generics
import System.Directory

import Development.Debugger.Flags
import Development.Debugger.Adaptor

import qualified Debugger
import qualified Debugger.Monad as Debugger
import Debugger.Interface.Messages hiding (Command, Response)
import qualified Debugger.Interface.Messages as D (Command, Response)

import DAP

--------------------------------------------------------------------------------
-- * Client
--------------------------------------------------------------------------------

-- | Client arguments are custom for launch
data LaunchArgs
  = LaunchArgs
  { __sessionId :: Maybe String
    -- ^ SessionID, set by VSCode client
  , projectRoot :: FilePath
    -- ^ Absolute path to the project root
  , entryFile :: FilePath
    -- ^ The file with the entry point e.g. @app/Main.hs@
  , entryPoint :: String
    -- ^ Either @main@ or a function name
  , entryArgs :: [String]
    -- ^ The arguments to either set as environment arguments when @entryPoint = "main"@
    -- or function arguments otherwise.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON

--------------------------------------------------------------------------------
-- * Launch Debugger
--------------------------------------------------------------------------------

-- | Initialize debugger
--
-- Todo:
-- [ ] Consider exception handling leading to termination
initDebugger :: LaunchArgs -> DebugAdaptor ()
initDebugger LaunchArgs{__sessionId, projectRoot, entryFile, entryPoint, entryArgs} = do

  syncRequests  <- liftIO newEmptyMVar
  syncResponses <- liftIO newEmptyMVar
  flags <- liftIO $ hieBiosFlags projectRoot entryFile

  let nextFreshBreakpointId = 0
      breakpointMap = mempty

  finished_init <- liftIO $ newEmptyMVar

  registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{..}
    [ debuggerThread finished_init projectRoot flags syncRequests syncResponses
    , outputEventsThread
    ]

  -- Do not return until the initialization is finished
  liftIO $ takeMVar finished_init

-- | The main debugger thread launches a GHC.Debugger session.
--
-- Then, forever:
--  1. Reads commands from the given 'D.Command' 'MVar'
--  2. Executes the command with `execute`
--  3. Writes responses to the given 'D.Response' 'MVar'
--
-- Concurrently, it reads from the process's stderr forever and outputs it through OutputEvents.
--
-- Notes:
--
-- (CWD):
--    It's necessary for the GHC session to be run in the project root.
--    We do this by setting the current directory on initialize.
--    This sets the global CWD for this process, disallowing multiple sessions
--    at the same, but that's OK because we currently only support
--    single-session mode. Each new session gets a new debugger process.
debuggerThread :: MVar ()         -- ^ To signal when initialization is complete.
               -> FilePath        -- ^ Working directory for GHC session
               -> HieBiosFlags    -- ^ GHC Invocation flags
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> (DebugAdaptorCont () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread finished_init workDir HieBiosFlags{..} requests replies withAdaptor = do

  -- See Notes (CWD) above
  setCurrentDirectory workDir

  -- Log ghc-debugger invocation
  withAdaptor $
    sendConsoleEvent $ T.pack $
      "libdir: " <> libdir <> "\n" <>
      "units: " <> unwords units <> "\n" <>
      "args: " <> unwords ghcInvocation

  Debugger.runDebugger libdir units ghcInvocation $ do
    putMVar finished_init ()   & liftIO
 
    forever $ do
      req <- takeMVar requests & liftIO
      resp <- (Debugger.execute req <&> Right)
                `catch` \(e :: SomeException) -> pure (Left (displayExceptionWithContext e))
      either bad reply resp

  where
    reply = liftIO . putMVar replies
    bad m = liftIO $ do
      hPutStrLn stderr m
      putMVar replies (Aborted m)

-- | The process's output is continuously read from its stderr and written to the DAP Client as OutputEvents.
-- This thread is responsible for this.
outputEventsThread :: (DebugAdaptorCont () -> IO ())
                   -- ^ Unlift DebugAdaptor action to send output events.
                   -> IO ()
outputEventsThread withAdaptor = do
  -- Read the debugger output from stderr (stdout is also redirected to stderr),
  -- And emit Output Events,
  -- Forever.
  forever $ do
    -- TODO: This should work without hGetLine
    line <- T.hGetLine stdout
    withAdaptor $ sendConsoleEvent line

--- Utils ----------------------------------------------------------------------

-- | Display an exception with its context
displayExceptionWithContext :: SomeException -> String
displayExceptionWithContext ex = do
  case displayExceptionContext (someExceptionContext ex) of
    "" -> displayException ex
    cx -> displayException ex ++ "\n\n" ++ cx
