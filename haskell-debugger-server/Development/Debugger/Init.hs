{-# LANGUAGE OverloadedStrings, RecordWildCards, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}

-- | TODO: This module should be called Launch.
module Development.Debugger.Init where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Aeson as Aeson
import GHC.Generics

import Development.Debugger.Flags
import Development.Debugger.Adaptor

import Debugger.Interface.Messages hiding (Command, Response)
import qualified Debugger.Interface.Messages as D (Command, Response)
import qualified System.Process as P
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

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

  -- Pipe for debugger output. One end goes to 'debuggerThread' to initiate the
  -- process, the other goes to 'outputEventsThread' to read the output out.
  (readDebuggerOutput, writeDebuggerOutput) <- liftIO P.createPipe
  forM_ [ readDebuggerOutput, writeDebuggerOutput ] $ \h -> liftIO $ do
    h `hSetBuffering` LineBuffering
    h `hSetEncoding` utf8

  finished_mvar <- liftIO $ newEmptyMVar

  registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{..}
    [ debuggerThread finished_mvar projectRoot flags syncRequests syncResponses writeDebuggerOutput
    , outputEventsThread readDebuggerOutput

    -- , if we make the debugger emit events (rather than always replying synchronously),
    -- we will listen for them here to forward them to the client.
    ]
  -- Do not return until the
  liftIO $ takeMVar finished_mvar

-- | The main debugger thread launches a `ghc-debugger` process.
--
-- Then, forever:
--  1. Reads commands from the given 'D.Command' 'MVar'
--  2. Sends them to the process's stdin
--  3. Reads responses from stdout
--  4. Writes responses to the given 'D.Response' 'MVar'
--
-- Concurrently, it reads from the process's stderr forever and outputs it through OutputEvents.
--
--  TODO:
--    [ ] Detect the process crashes?
--    [ ] Intercept "Abort" commands and kill the process
--    [ ] Make sure "Abort" commands are sent here when session should terminate.
--    [ ] Receive the ghc-debugger executable path as an argument or at least env variable.
--
-- Notes:
--  * It's necessary for the GHC session to be run in the project root.
--    Launching a separate process allows a concurrent DAP sessions with
--    appropriate current working directories for simultaneous GHC sessions.
debuggerThread :: MVar ()
               -> FilePath        -- ^ Working directory for GHC session
               -> HieBiosFlags    -- ^ GHC Invocation flags
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> Handle          -- ^ The handle to which the debugger will write the output
               -> (DebugAdaptor () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread finished_mvar workDir HieBiosFlags{..} requests replies writeDebuggerOutput withAdaptor = do

  (readFromServer, writeToDebugger) <- P.createPipe
  (readFromDebugger, writeToServer) <- P.createPipe
  forM_ [ readFromServer, writeToDebugger
        , readFromDebugger, writeToServer
        ] $ \h -> do
    h `hSetBuffering` LineBuffering
    h `hSetEncoding` utf8

  let cmd = "ghc-debugger" -- FIXME: read path to executable from somewhere.
  let args = ["-B" ++ libdir]
              ++ concatMap (\u -> ["-unit", u]) units
              ++ ghcInvocation

  -- Log ghc-debugger invocation
  withAdaptor $
    sendConsoleEvent $ T.pack $
      cmd <> " " <> unwords args

  ghcDebuggerProc <-
    P.runProcess cmd args
                 (Just workDir) Nothing
                 (Just readFromServer)      -- stdin
                 (Just writeToServer)       -- stdout
                 (Just writeDebuggerOutput) -- stderr

  resp <- BS8.hGetLine readFromDebugger >>= pure . Aeson.eitherDecodeStrict
  case resp of
    Right Initialised -> do
      putMVar finished_mvar ()
    _ -> error ("Unexpected response" ++ (show resp))

  forever $ do
    req <- takeMVar requests
    -- TODO: Read/Write without newline buffering?
    BSL8.hPutStrLn writeToDebugger (Aeson.encode req)
    resp <- BS8.hGetLine readFromDebugger >>= pure . Aeson.eitherDecodeStrict
    either bad reply resp

  where
    reply = putMVar replies
    bad m = do
      hPutStrLn stderr m
      putMVar replies (Aborted m)

-- | The process's output is continuously read from its stderr and written to the DAP Client as OutputEvents.
-- This thread is responsible for this.
outputEventsThread :: Handle
                   -- ^ The handle from which we can read the debugger output
                   -> (DebugAdaptor () -> IO ())
                   -- ^ Unlift DebugAdaptor action to send output events.
                   -> IO ()
outputEventsThread readDebuggerOutput withAdaptor = do
  -- Read the debugger output from stderr (stdout is also redirected to stderr),
  -- And emit Output Events,
  -- Forever.
  forever $ do
    -- TODO: This should work without hGetLine
    line <- T.hGetLine readDebuggerOutput
    withAdaptor $ sendConsoleEvent line
