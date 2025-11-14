{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | TODO: This module should be called Launch.
module Development.Debug.Adapter.Init where

import GHC.IO.Handle
import System.Process
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified System.Process as P
import Control.Monad.Except
import Control.Monad.Trans
import Data.Function
import Data.Functor
import Data.Maybe
import System.IO
import GHC.IO.Encoding
import Control.Monad.Catch
import Control.Exception (SomeAsyncException, throwIO)
import Control.Concurrent
import Control.Monad
import Data.Aeson as Aeson
import GHC.Generics
import System.Directory
import System.FilePath

import Development.Debug.Adapter
import Development.Debug.Adapter.Exit
import GHC.Debugger.Logger
import qualified Development.Debug.Adapter.Output as Output

import qualified GHC.Debugger as Debugger
import qualified GHC.Debugger.Monad as Debugger
import qualified GHC.Debugger.Interface.Messages as D (Command, Response)
import GHC.Debugger.Interface.Messages hiding (Command, Response)

import DAP
import Development.Debug.Adapter.Handles
import Development.Debug.Session.Setup

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

data InitLog
  = DebuggerLog Debugger.DebuggerLog
  | DebuggerMonadLog Debugger.DebuggerMonadLog
  | FlagsLog FlagsLog

instance Pretty InitLog where
  pretty = \ case
    DebuggerLog msg -> pretty msg
    DebuggerMonadLog msg -> pretty msg
    FlagsLog msg -> pretty msg

--------------------------------------------------------------------------------
-- * Client
--------------------------------------------------------------------------------

-- | Client arguments are custom for launch
data LaunchArgs
  = LaunchArgs
  { __sessionId :: Maybe String
    -- ^ SessionID, set by VSCode client
  , projectRoot :: Maybe FilePath
    -- ^ Absolute path to the project root
  , entryFile :: Maybe FilePath
    -- ^ The file with the entry point e.g. @app/Main.hs@
  , entryPoint :: Maybe String
    -- ^ Either @main@ or a function name
  , entryArgs :: Maybe [String]
    -- ^ The arguments to either set as environment arguments when @entryPoint = "main"@
    -- or function arguments otherwise.
  , extraGhcArgs :: Maybe [String]
    -- ^ Additional arguments to pass to the GHC invocation inferred by hie-bios for this project
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON

--------------------------------------------------------------------------------
-- * Launch Debugger
--------------------------------------------------------------------------------

-- | Exception type for when hie-bios initialization fails
newtype InitFailed = InitFailed String deriving Show

-- | Initialize debugger
--
-- Returns @()@ if successful, throws @InitFailed@ otherwise
initDebugger :: Recorder (WithSeverity InitLog) -> Bool -> LaunchArgs -> ExceptT InitFailed DebugAdaptor ()
initDebugger l supportsRunInTerminal
               LaunchArgs{ __sessionId
                         , projectRoot = givenRoot
                         , entryFile = entryFileMaybe
                         , entryPoint = fromMaybe "main" -> entryPoint
                         , entryArgs  = fromMaybe [] -> entryArgs
                         , extraGhcArgs = fromMaybe [] -> extraGhcArgs
                         } = do
  syncRequests  <- liftIO newEmptyMVar
  syncResponses <- liftIO newEmptyMVar
  syncProxyIn   <- liftIO newChan
  syncProxyOut  <- liftIO newChan
  syncProxyErr  <- liftIO newChan

  entryFile <- case entryFileMaybe of
    Nothing -> throwError $ InitFailed "Missing \"entryFile\" key in debugger configuration"
    Just ef -> pure ef

  projectRoot <- maybe (liftIO getCurrentDirectory) pure givenRoot

  let hieBiosLogger = cmapWithSev FlagsLog l
  liftIO (runExceptT (hieBiosSetup hieBiosLogger projectRoot entryFile)) >>= \case
    Left e              -> throwError $ InitFailed e
    Right (Left e)      -> lift       $ exitWithMsg e
    Right (Right flags) -> do
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
        hSetBuffering readDebuggerOutput LineBuffering
        hSetBuffering writeDebuggerOutput NoBuffering
        -- GHC output uses utf8
        hSetEncoding readDebuggerOutput utf8
        hSetEncoding writeDebuggerOutput utf8
        setLocaleEncoding utf8

      finished_init <- liftIO $ newEmptyMVar

      let absEntryFile = normalise $ projectRoot </> entryFile
      lift $ registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{entryFile=absEntryFile,..}
        [ debuggerThread l finished_init writeDebuggerOutput projectRoot flags
            extraGhcArgs absEntryFile defaultRunConf syncRequests syncResponses
        , handleDebuggerOutput readDebuggerOutput
        , stdinForwardThread  supportsRunInTerminal syncProxyIn
        , stdoutCaptureThread supportsRunInTerminal syncProxyOut
        , stderrCaptureThread supportsRunInTerminal syncProxyErr
        ]

      -- Do not return until the initialization is finished
      liftIO (takeMVar finished_init) >>= \case
        Right () -> pure ()
        Left e   -> do
          -- The process terminates cleanly with an error code (probably exit failure = 1)
          -- This can happen if compilation fails and the compiler exits cleanly.
          --
          -- Instead of signalInitialized, respond with error and exit.
          lift $ exitCleanupWithMsg readDebuggerOutput e

-- | This thread captures stdout from the debuggee and sends it to the client.
-- NOTE, redirecting the stdout handle is a process-global operation. So this thread
-- will capture ANY stdout the debuggee emits. Therefore you should never directly
-- write to stdout, but always write to the appropiate handle.
stdoutCaptureThread :: Bool -> Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdoutCaptureThread runInTerminal syncOut withAdaptor = do
  withInterceptedStdout $ \_ interceptedStdout -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStdout
      when runInTerminal $
        writeChan syncOut $ T.encodeUtf8 (line <> T.pack "\n")

      -- Always output to Debug Console
      withAdaptor $ Output.stdout line

-- | Like 'stdoutCaptureThread' but for stderr
stderrCaptureThread :: Bool -> Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stderrCaptureThread runInTerminal syncErr withAdaptor = do
  withInterceptedStderr $ \_ interceptedStderr -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStderr
      when runInTerminal $
        writeChan syncErr $ T.encodeUtf8 (line <> "\n")

      -- Always output to Debug Console
      withAdaptor $ Output.stderr line

stdinForwardThread :: Bool -> Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdinForwardThread runInTerminal syncIn _withAdaptor = do
  when runInTerminal $ do
    -- We need to hijack stdin to write to it

    -- 1. Create a new pipe from writeEnd->readEnd
    (readEnd, writeEnd) <- createPipe

    -- 2. Substitute the read-end of the pipe by stdin
    _ <- hDuplicateTo readEnd stdin
    hClose readEnd -- we'll never need to read from readEnd

    forever $ do
      i <- readChan syncIn
      -- 3. Write to write-end of the pipe
      BS.hPut writeEnd i >> hFlush writeEnd

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
debuggerThread :: Recorder (WithSeverity InitLog)
               -> MVar (Either String ()) -- ^ To signal when initialization is complete.
               -> Handle          -- ^ The write end of a handle for debug compiler output
               -> FilePath        -- ^ Working directory for GHC session
               -> HieBiosFlags    -- ^ GHC Invocation flags
               -> [String]        -- ^ Extra ghc args
               -> FilePath
               -> Debugger.RunDebuggerSettings -- ^ Settings for running the debugger
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> (DebugAdaptorCont () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread recorder finished_init writeDebuggerOutput workDir HieBiosFlags{..} extraGhcArgs mainFp runConf requests replies withAdaptor = do

  let finalGhcInvocation = ghcInvocation ++ extraGhcArgs

  -- See Notes (CWD) above
  setCurrentDirectory workDir

  -- Log haskell-debugger invocation
  withAdaptor $
    Output.console $ T.pack $
      "libdir: " <> libdir <> "\n" <>
      "units: " <> unwords units <> "\n" <>
      "args: " <> unwords finalGhcInvocation

  catches
    (do
      Debugger.runDebugger (cmapWithSev DebuggerMonadLog recorder) writeDebuggerOutput rootDir componentDir libdir units finalGhcInvocation mainFp runConf $ do
        liftIO $ signalInitialized (Right ())
        forever $ do
          req <- takeMVar requests & liftIO
          resp <- (Debugger.execute (cmapWithSev DebuggerLog recorder) req <&> Right)
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

-- | Reads from the read end of the handle to which the debugger writes compiler messages.
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

