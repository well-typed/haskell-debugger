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
import GHC.Conc.Sync (labelThread)
import Control.Monad
import Data.Aeson as Aeson
import GHC.Generics
import System.Directory
import System.FilePath
import Data.Functor.Contravariant

import Development.Debug.Adapter
import Development.Debug.Adapter.Exit
import Colog.Core as Logger
import qualified Development.Debug.Adapter.Output as Output

import GHC.Utils.Logger (defaultLogActionWithHandles)
import GHC.Debugger.Utils (forwardHandleToLogger)
import qualified GHC.Debugger as Debugger
import qualified GHC.Debugger.Monad as Debugger
import qualified GHC.Debugger.Interface.Messages as D (Command, Response)
import GHC.Debugger.Interface.Messages hiding (Command, Response)

import DAP
import Development.Debug.Adapter.Handles
import Development.Debug.Session.Setup

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
-- * Logging
--------------------------------------------------------------------------------

data DAPLog
  = DAPSessionSetupLog (WithSeverity SessionSetupLog)
  | DAPDebuggerLog Debugger.DebuggerLog

--------------------------------------------------------------------------------
-- * Launch Debugger
--------------------------------------------------------------------------------


-- | Exception type for when hie-bios initialization fails
newtype InitFailed = InitFailed String deriving Show

-- | Initialize debugger
--
-- Returns @()@ if successful, throws @InitFailed@ otherwise
initDebugger :: LogAction IO DAPLog -> Bool -> Bool
             -> LaunchArgs -> ExceptT InitFailed DebugAdaptor ()
initDebugger l supportsRunInTerminal preferInternalInterpreter
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

  let hieBiosLogger = contramap DAPSessionSetupLog l
  liftIO (runExceptT (hieBiosSetup hieBiosLogger projectRoot entryFile)) >>= \case
    Left e              -> throwError $ InitFailed e
    Right (Left e)      -> lift       $ exitWithMsg e
    Right (Right flags) -> do
      -- Create the stdin handle for the external interpreter
      (readExternalIntStdin, writeExternalIntStdin) <- liftIO P.createPipe
      liftIO $ do
        hSetBuffering readExternalIntStdin LineBuffering
        hSetBuffering writeExternalIntStdin NoBuffering
        hSetEncoding readExternalIntStdin utf8
        hSetEncoding writeExternalIntStdin utf8

      let nextFreshId = 0
          breakpointMap = mempty
          stackFrameMap = mempty
          variablesMap  = mempty
          defaultRunConf = Debugger.RunDebuggerSettings
            { supportsANSIStyling = True     -- TODO: Initialize Request sends supportsANSIStyling; this is False for nvim-dap
            , supportsANSIHyperlinks = False -- VSCode does not support this
            , preferInternalInterpreter
            , externalInterpreterStdinStream = UseHandle readExternalIntStdin
            }

      -- Create a pipe to which messages to send to the DAP console are written and read.
      -- todo: This could just be a Haskell channel now...
      (readDAPOutput, writeDAPOutput) <- liftIO P.createPipe
      liftIO $ do
        hSetBuffering readDAPOutput LineBuffering
        hSetBuffering writeDAPOutput NoBuffering
        -- GHC output uses utf8
        hSetEncoding readDAPOutput utf8
        hSetEncoding writeDAPOutput utf8
        setLocaleEncoding utf8

      finished_init <- liftIO $ newEmptyMVar

      dbgLog <- liftIO $
        createDebuggerLogger l writeDAPOutput (supportsRunInTerminal, syncProxyOut, syncProxyErr)

      let absEntryFile = normalise $ projectRoot </> entryFile
      lift $ registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{entryFile=absEntryFile,..}
        [ debuggerThread dbgLog finished_init projectRoot flags
            extraGhcArgs absEntryFile defaultRunConf syncRequests syncResponses

        , \withAdaptor -> forwardHandleToLogger readDAPOutput $
            LogAction (\msg -> withAdaptor (Output.neutral msg))

        -- Setup capturing of the process' own stdout and forwarding of the process' own stdin,
        -- but only if we're using the internal interpreter!
        , if preferInternalInterpreter then stdinForwardThread  supportsRunInTerminal syncProxyIn  else mempty
        , if preferInternalInterpreter then stdoutCaptureThread supportsRunInTerminal syncProxyOut else mempty
        , if preferInternalInterpreter then stderrCaptureThread supportsRunInTerminal syncProxyErr else mempty
        , if preferInternalInterpreter
            then mempty
            else const $ do
              -- forward proxy in to external interpreter stdin
              forever $ do
                i <- readChan syncProxyIn
                -- 3. Write to write-end of the pipe
                BS.hPut writeExternalIntStdin i >> hFlush writeExternalIntStdin
        ]

      -- Do not return until the initialization is finished
      liftIO (takeMVar finished_init) >>= \case
        Right () -> pure ()
        Left e   -> do
          -- The process terminates cleanly with an error code (probably exit failure = 1)
          -- This can happen if compilation fails and the compiler exits cleanly.
          --
          -- Instead of signalInitialized, respond with error and exit.
          lift $ exitCleanupWithMsg readDAPOutput e

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
debuggerThread :: LogAction IO Debugger.DebuggerLog
               -> MVar (Either String ()) -- ^ To signal when initialization is complete.
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
debuggerThread l finished_init workDir HieBiosFlags{..} extraGhcArgs mainFp runConf requests replies withAdaptor = do

  -- See Notes (CWD) above
  setCurrentDirectory workDir

  -- Log haskell-debugger invocation
  withAdaptor $
    Output.console $ T.pack $
      "libdir: " <> libdir <> "\n" <>
      "units: " <> unwords units <> "\n" <>
      "args: " <> unwords (ghcInvocation ++ extraGhcArgs)

  catches
    (do
      Debugger.runDebugger l rootDir componentDir libdir units ghcInvocation extraGhcArgs mainFp runConf $ do
        liftIO $ do
          tid <- myThreadId
          labelThread tid "Main Debugger Thread"
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
      putMVar replies (Aborted ("Aborted debugger thread: " ++ m))

--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------
{-
Note [Debugger, debuggee, and DAP logs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Specification for the logger given to `Debugger`:

1. All -v3 DebuggerLog and GHCLog messages go to the normal stdout/stderr (this shows up in
  the OUTPUT console in VSCode, without having to send special messages)

2. All -v1 DebuggerLog, GHCLog, and all LogDebuggeeOut and LogDebuggeeErr output
  goes to the DAP console (this is DEBUG CONSOLE in VSCode)

3. All LogDebuggeeOut and LogDebuggeeErr output are forwarded to the proxy if
  the proxy is enabled.
-}

-- See Note [Debugger, debuggee, and DAP logs]
createDebuggerLogger
  :: LogAction IO DAPLog
  -> Handle -- ^ Handle to write to DAP output
  -> (Bool, Chan BS.ByteString, Chan BS.ByteString) -- ^ Proxy channels, and whether is supported
  -> IO (LogAction IO Debugger.DebuggerLog)
createDebuggerLogger l writeDAPOutput (supportsRunInTerminal, syncProxyOut, syncProxyErr) = do
  dapLogger <- handleLogger writeDAPOutput
  return $
    -- (1) (all output is logged to normal logger)
    contramap DAPDebuggerLog l <>
    -- (2) and (3) (log relevant output to DAP handle)
      LogAction (\case
        Debugger.DebuggerLog sev msg
          | sev >= Info -> do
            dapLogger <& T.pack (show msg)
        Debugger.GHCLog logflags msg_class srcSpan msg ->
          defaultLogActionWithHandles writeDAPOutput writeDAPOutput logflags msg_class srcSpan msg
        Debugger.LogDebuggeeOut txt -> debuggeeOut dapLogger syncProxyOut txt
        Debugger.LogDebuggeeErr txt -> debuggeeOut dapLogger syncProxyErr txt
        _ -> pure () -- don't log other messages, already logged to (1)
        )
  where
    debuggeeOut l' proxyChan txt = do
      -- (2)
      l' <& txt
      -- (3)
      when supportsRunInTerminal $
        writeChan proxyChan $
          T.encodeUtf8 (txt <> "\n")

--------------------------------------------------------------------------------
-- * Capturing stdout, stderr, and writing to self stdin
--------------------------------------------------------------------------------

-- | Hijack the current process stdin and forward to it the messages from the given channel
stdinForwardThread :: Bool -> Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdinForwardThread runInTerminal syncIn _withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stdin Forward Thread"
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

-- | This thread captures stdout from the debuggee and sends it to the client.
-- NOTE, redirecting the stdout handle is a process-global operation. So this thread
-- will capture ANY stdout the debuggee emits. Therefore you should never directly
-- write to stdout, but always write to the appropiate handle.
stdoutCaptureThread :: Bool -> Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdoutCaptureThread runInTerminal syncOut withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stdin Capture Thread"
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
  tid <- myThreadId
  labelThread tid "Stderr Capture Thread"
  withInterceptedStderr $ \_ interceptedStderr -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStderr
      when runInTerminal $
        writeChan syncErr $ T.encodeUtf8 (line <> "\n")

      -- Always output to Debug Console
      catch
        (withAdaptor $ Output.stderr line)
        (\(_ :: SomeException) ->
          throwIO (FailedToWriteToAdaptor line))

newtype FailedToWriteToAdaptor = FailedToWriteToAdaptor T.Text
instance Show FailedToWriteToAdaptor where
  show (FailedToWriteToAdaptor t) = "Failed to write to debug adapter: " ++ T.unpack t
instance Exception FailedToWriteToAdaptor
