{-# LANGUAGE OverloadedStrings, RecordWildCards, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
module Development.Debugger.Init where

import Control.Exception
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
import Development.Debugger.Exit

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

  registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{..}
    -- The following threads will all be run concurrently.
    -- If `destroyDebugSession` is called, they will be killed
    [ debuggerThread projectRoot flags syncRequests syncResponses (readDebuggerOutput, writeDebuggerOutput)
    , readOutputsThread readDebuggerOutput
    -- , if we make the debugger emit events (rather than always replying synchronously),
    -- we will listen for them here to forward them to the client.
    ]

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
debuggerThread :: FilePath         -- ^ Working directory for GHC session
               -> HieBiosFlags     -- ^ GHC Invocation flags
               -> MVar D.Command   -- ^ Read commands
               -> MVar D.Response  -- ^ Write reponses
               -> (Handle, Handle) -- ^ The handle from/to which the debugger will read/write the output
               -> (DebugAdaptor () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread workDir HieBiosFlags{..}
  requests replies (readDebuggerOutput, writeDebuggerOutput)
    withAdaptor = flip catch (withAdaptor . handleDebuggerException readDebuggerOutput) $ do

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

  -- Since `destroyDebugSession` may kill this thread, make sure to intercept
  -- the signal and kill the underlying process too.
  ghcDebuggerProc <-
    P.runProcess cmd args
                 (Just workDir) Nothing
                 (Just readFromServer)      -- stdin
                 (Just writeToServer)       -- stdout
                 (Just writeDebuggerOutput) -- stderr
  (do
    -- We'll try to process requests and responses to the debugger process forever.
    --
    -- If the debugger process crashes for some reason (e.g. compilation fails)
    -- then the process handle will be closed.
    -- We catch this exception with `handleDebuggerException`.
    forever $ do
      req <- takeMVar requests

      resp <- (do
        -- TODO: Read/Write without newline buffering?
        BSL8.hPutStrLn writeToDebugger (Aeson.encode req)
        BS8.hGetLine readFromDebugger
          >>= pure . Aeson.eitherDecodeStrict
        ) `catch` \(e :: SomeException) -> do
          -- Catch ensures a response is always written (otherwise e.g.
          -- sendSync may deadlock)
          pure $ Left $ displayExceptionWithContext e
      either bad reply resp
    ) `onException` do
        -- Always terminate the process!
        withAdaptor $ logInfo $ BSL8.pack "Terminating ghc-debugger process..."
        P.terminateProcess ghcDebuggerProc
        withAdaptor $ logInfo $ BSL8.pack "Process terminated!"

  where
    reply = putMVar replies
    bad m = putMVar replies (Aborted m)

-- | The process's output is continuously read from the given handle and written to the given Output channel.
--
-- WARNING: It is critical that the thread which reads from the handle does NOT
-- try to send console events, because sending events requires taking an
-- adapter lock and we may enter a deadlock as follows:
--
-- 1. Initialisation starts, starts writing to a pipe
-- 2. Worker thread tries to output things from the pipe, but can't because that requires taking a lock on the MVar
-- 3. Step 1 blocks because the pipe gets full
-- 4. Step 2 blocks, since it is waiting for step 1 to finish
--
readOutputsThread :: Handle
                  -- ^ The handle from which we can read the debugger output
                  -> (DebugAdaptor () -> IO ())
                  -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
                  -> IO ()
readOutputsThread readDebuggerOutput withAdaptor
    = flip catch (\(_::SomeException) -> pure ()) $ do
      -- ignore all exceptions in this thread and simply die.
      -- `handleDebuggerException` will handle the same failures (process
      -- dying) in `debuggerThread`.

  -- Read the debugger output from stderr (stdout is also redirected to stderr),
  -- And put the output in the channel forever
  forever $
    -- Disallow this thread to be killed between getting the line and
    -- sending the console event. Ensures the output can all be flushed before
    -- terminating.
    mask_ $ do
      -- TODO: This should work without hGetLine (ie hGet)
      line <- T.hGetLine readDebuggerOutput
      withAdaptor $ sendConsoleEvent line

