{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}

-- | TODO: This module should be called Launch.
module Development.Debugger.Init where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Process as P
import Data.Function
import Data.Functor
import Control.Monad.IO.Class
import System.IO
import GHC.IO.Encoding
import Control.Monad.Catch
import Control.Exception (SomeAsyncException, throwIO)
import Control.Concurrent
import Control.Monad
import Data.Aeson as Aeson
import GHC.Generics
import System.Directory

import Development.Debugger.Flags
import Development.Debugger.Adaptor
import qualified Development.Debugger.Output as Output
import Development.Debugger.Exit

import qualified Debugger
import qualified Debugger.Monad as Debugger
import Debugger.Interface.Messages hiding (Command, Response)
import qualified Debugger.Interface.Messages as D (Command, Response)

import DAP
import Handles

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
-- Returns @True@ if successful.
initDebugger ::  LaunchArgs -> DebugAdaptor Bool
initDebugger LaunchArgs{__sessionId, projectRoot, entryFile, entryPoint, entryArgs} = do
  syncRequests  <- liftIO newEmptyMVar
  syncResponses <- liftIO newEmptyMVar

  Output.console $ T.pack "Discovering session flags with hie-bios..."
  mflags <- liftIO (hieBiosFlags projectRoot entryFile)
  case mflags of
    Left e -> do exitWithMsg e
                 return False
    Right flags -> do

      let nextFreshBreakpointId = 0
          breakpointMap = mempty
          defaultRunConf = Debugger.RunDebuggerSettings
            { supportsANSIStyling = True     -- TODO: Initialize Request sends supportsANSIStyling; this is False for nvim-dap
            , supportsANSIHyperlinks = False -- VSCode does not support this
            }


      -- Create pipes to read/write the debugger (not debuggee's) output.
      -- The write end is given to `runDebugger` and the read end is continuously
      -- read from until we read an EOF.
      (readDebuggerOutput, writeDebuggerOutput) <- liftIO P.createPipe
      liftIO $ do
        hSetBuffering readDebuggerOutput NoBuffering
        hSetBuffering writeDebuggerOutput NoBuffering
        -- GHC output uses utf8
        hSetEncoding readDebuggerOutput utf8
        hSetEncoding writeDebuggerOutput utf8
        setLocaleEncoding utf8

      finished_init <- liftIO $ newEmptyMVar

      registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{..}
        [ debuggerThread finished_init writeDebuggerOutput projectRoot flags defaultRunConf syncRequests syncResponses
        , handleDebuggerOutput readDebuggerOutput
        , stdoutCaptureThread
        ]

      -- Do not return until the initialization is finished
      liftIO (takeMVar finished_init) >>= \case
        Right () -> return True
        Left e   -> do
          -- The process terminates cleanly with an error code (probably exit failure = 1)
          -- This can happen if compilation fails and the compiler exits cleanly.
          --
          -- Instead of signalInitialized, respond with error and exit.
          exitCleanupWithMsg readDebuggerOutput e
          return False

-- | This thread captures stdout from the debugger and sends it to the client.
-- NOTE, redirecting the stdout handle is a process-global operation. So this thread
-- will capture ANY stdout the debugger emits. Therefore you should never directly
-- write to stdout, but always write to the appropiate handle.
stdoutCaptureThread :: (DebugAdaptorCont () -> IO ()) -> IO ()
stdoutCaptureThread withAdaptor = do
  withInterceptedStdout $ \_ interceptedStdout -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStdout
      withAdaptor $ Output.stdout line

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
debuggerThread :: MVar (Either String ()) -- ^ To signal when initialization is complete.
               -> Handle          -- ^ The write end of a handle for debug compiler output
               -> FilePath        -- ^ Working directory for GHC session
               -> HieBiosFlags    -- ^ GHC Invocation flags
               -> Debugger.RunDebuggerSettings -- ^ Settings for running the debugger
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> (DebugAdaptorCont () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread finished_init writeDebuggerOutput workDir HieBiosFlags{..} runConf requests replies withAdaptor = do

  -- See Notes (CWD) above
  setCurrentDirectory workDir

  -- Log ghc-debugger invocation
  withAdaptor $
    Output.console $ T.pack $
      "libdir: " <> libdir <> "\n" <>
      "units: " <> unwords units <> "\n" <>
      "args: " <> unwords ghcInvocation

  catches
    (do
      Debugger.runDebugger writeDebuggerOutput libdir units ghcInvocation runConf $ do
        liftIO $ signalInitialized (Right ())
     
        forever $ do
          req <- takeMVar requests & liftIO
          resp <- (Debugger.execute req <&> Right)
                    `catch` \(e :: SomeException) ->
                        pure (Left (displayExceptionWithContext e))
          either bad reply resp
    )
    [ Handler $ \(e::SomeAsyncException) -> do
        throwIO e
    , Handler $ \(e::SomeException) -> do
        signalInitialized (Left (displayException e))
    ]

  where
    signalInitialized
          = putMVar finished_init
    reply = liftIO . putMVar replies
    bad m = liftIO $ do
      hPutStrLn stderr m
      putMVar replies (Aborted m)

-- | Reads from the read end of the handle to which the GHC debugger writes compiler messages.
-- Writes the compiler messages to the client console
handleDebuggerOutput :: Handle
                     -> (DebugAdaptorCont () -> IO ())
                     -> IO ()
handleDebuggerOutput readDebuggerOutput withAdaptor = do

  -- Mask exceptions to avoid being killed between reading a line and outputting it.
  (forever $ mask_ $ do
    line <- T.hGetLine readDebuggerOutput
    withAdaptor $ Output.neutral line
    ) `catch` -- handles read EOF
        \(_e::SomeException) ->
          -- Cleanly exit when readDebuggerOutput is closed or thread is killed.
          return ()
