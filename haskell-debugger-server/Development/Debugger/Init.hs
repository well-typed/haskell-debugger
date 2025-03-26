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

  registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{..}
    [ debuggerThread projectRoot flags syncRequests syncResponses

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
debuggerThread :: FilePath        -- ^ Working directory for GHC session
               -> HieBiosFlags    -- ^ GHC Invocation flags
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> (DebugAdaptor () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread workDir HieBiosFlags{..} requests replies withAdaptor = do

  (readFromServer, writeToDebugger) <- P.createPipe
  (readFromDebugger, writeToServer) <- P.createPipe
  (readDebuggerOutput, writeDebuggerOutput) <- P.createPipe
  forM_ [ readFromServer, writeToDebugger
        , readFromDebugger, writeToServer
        , readDebuggerOutput, writeDebuggerOutput
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

  -- Read the debugger output from stderr (stdout is also redirected to stderr),
  -- And emit Output Events,
  -- Forever.
  _ <- forkIO $ do
    forever $ do
      line <- T.hGetLine readDebuggerOutput
      withAdaptor $ sendConsoleEvent line


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

