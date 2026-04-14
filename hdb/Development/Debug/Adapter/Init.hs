{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | TODO: This module should be called Launch.
module Development.Debug.Adapter.Init where

#if !MIN_VERSION_ghc(9,15,0)
-- no longer needs to be imported from here in 9.15
import GHC.Conc.Sync (labelThread)
#endif

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
import Data.UUID.V4 qualified as UUID
import System.IO
import GHC.IO.Encoding
import Control.Monad.Catch
import Control.Exception (SomeAsyncException, throwIO, IOException)
import Control.Concurrent
import Control.Monad
import Data.Aeson as Aeson
import GHC.Generics
import System.Directory
import System.FilePath
import Data.Functor.Contravariant

import Development.Debug.Adapter
import Development.Debug.Adapter.Exit.Helpers
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
import Development.Debug.Adapter.Proxy
import System.Environment
import Network.Socket (socketPort)

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
  | RunProxyServerLog (WithSeverity T.Text)

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

  entryFile <- case entryFileMaybe of
    Nothing -> throwError $ InitFailed "Missing \"entryFile\" key in debugger configuration"
    Just ef -> pure ef

  projectRoot <- maybe (liftIO getCurrentDirectory) pure givenRoot

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

  dapLogger <- liftIO $ handleLogger writeDAPOutput

  let hieBiosLogger = contramap DAPSessionSetupLog l <> logHieBiosToDAP

      logHieBiosToDAP = LogAction $ \case
        WithSeverity msg sev
          | sev >= Info -> dapLogger <& renderSessionSetupLog msg
          | otherwise -> mempty

  liftIO (runExceptT (hieBiosSetup hieBiosLogger projectRoot entryFile)) >>= \case
    Left e              -> throwError $ InitFailed e
    Right (Left e)      -> lift       $ terminateWithError e
    Right (Right flags) -> do

      let
        nextFreshId = 0
        breakpointMap = mempty
        stackFrameMap = mempty
        variablesMap  = mempty

        mkRunInTerminalProc
          | not supportsRunInTerminal
          = pure NoRunInTerminal
          | not preferInternalInterpreter
          = do
            sock <- openSocketAvailablePort
            port <- socketPort sock
            pure RunExternalInterpreterInTerminal
              { extInterpPort  = port
              }
          | otherwise
          = do
            (syncProxyIn, syncProxyOut, syncProxyErr)
                             <- (,,) <$> newChan <*> newChan <*> newChan
            proxyClientReady <- newEmptyMVar
            pure RunProxyInTerminal{..}

      runInTerminalProc <- liftIO mkRunInTerminalProc

      dbgLog <- liftIO $
        createDebuggerLogger l dapLogger writeDAPOutput runInTerminalProc

      (runInTerminalThreads, afterRegisterActions) <- lift $
        mkRunInTerminalThreads l runInTerminalProc preferInternalInterpreter

      let
        defaultRunConf = Debugger.RunDebuggerSettings
          { supportsANSIStyling = True     -- TODO: Initialize Request sends supportsANSIStyling; this is False for nvim-dap
          , supportsANSIHyperlinks = False -- VSCode does not support this
          , preferInternalInterpreter
          , externalInterpreterCustomProc = case runInTerminalProc of
              RunExternalInterpreterInTerminal{extInterpPort}
                -> Right extInterpPort
              _ -> Left CreatePipe -- if not runInTerminal, just create a new pipe for stdin
          }
        absEntryFile = normalise $ projectRoot </> entryFile
        daState = DAS{entryFile=absEntryFile,..}

      sessionId <- liftIO $ maybe (("debug-session:" <>) . T.show <$> UUID.nextRandom) (pure . T.pack) __sessionId
      lift $ registerNewDebugSession sessionId daState $
        [ debuggerThread dbgLog flags extraGhcArgs absEntryFile defaultRunConf syncRequests syncResponses
        , \withAdaptor -> forwardHandleToLogger readDAPOutput $
            LogAction (\msg -> withAdaptor (Output.neutral msg))
        ]
        ++
        runInTerminalThreads

      lift afterRegisterActions

-- | Additional threads to register for this session depending on the process
-- we're running through `runInTerminal` (see 'RunInTerminalProc').
mkRunInTerminalThreads
  :: LogAction IO DAPLog
  -> RunInTerminalProc
  -> Bool -- ^ Use internal interpreter
  -> DebugAdaptor ([(DebugAdaptorCont () -> IO ()) -> IO ()], DebugAdaptor ())
    -- ^ Threads to register in this debug session and additional commands to
    -- run after registering the session.

mkRunInTerminalThreads _ NoRunInTerminal useInternalInterp
  -- Not using the terminal proxy, but we still want to output our own
  -- stdout/err (from the internal interpreter) as console events.
  | True <- useInternalInterp
  = pure ([ stdoutCaptureThread Nothing, stderrCaptureThread Nothing ], pure ())

  | otherwise
  = pure ([], pure ())

mkRunInTerminalThreads _ RunExternalInterpreterInTerminal{..} _
  -- No additional bookkeeping is needed in this case because GHC will
  -- naturally have to wait for the external interpreter in order to start execution
  = do
  thisProg <- liftIO getExecutablePath -- run the same `hdb` executable in `proxy` mode
  pure ([],
    sendRunInTerminalReverseRequest
      RunInTerminalRequestArguments
        { runInTerminalRequestArgumentsKind = Just RunInTerminalRequestArgumentsKindIntegrated
        , runInTerminalRequestArgumentsTitle = Nothing
        , runInTerminalRequestArgumentsCwd = ""
        , runInTerminalRequestArgumentsArgs =
            [T.pack thisProg, "external-interpreter", "--port", T.pack (show extInterpPort)]
        , runInTerminalRequestArgumentsEnv = Nothing
        , runInTerminalRequestArgumentsArgsCanBeInterpretedByShell = False
        })

mkRunInTerminalThreads l RunProxyInTerminal{..} _
  = do
    (serverPort, serverProxyThread) <-
      mkServerSideHdbProxy (contramap RunProxyServerLog l)
        syncProxyIn syncProxyOut syncProxyErr proxyClientReady

    pure (
      [ ($ serverProxyThread)

      -- Setup capturing of the process' own stdout and forwarding of the process' own stdin,
      -- but only because we're using the internal interpreter!
      , stdinForwardThread  syncProxyIn
      , stdoutCaptureThread (Just syncProxyOut)
      , stderrCaptureThread (Just syncProxyErr)
      ],

      -- When using the internal interpreter and 'runInTerminal' is supported
      -- (the 'RunProxyInTerminal' case), we ask the DAP client to launch the
      -- `hdb proxy` attached to the user's terminal. The proxy forwards
      -- input/output from the user terminal to the debugger+debuggee shared process
      sendRunProxyInTerminal serverPort
      )

-- | The main debugger thread launches a GHC.Debugger session.
--
-- Then, forever:
--  1. Reads commands from the given 'D.Command' 'MVar'
--  2. Executes the command with `execute`
--  3. Writes responses to the given 'D.Response' 'MVar'
--
-- Concurrently, it reads from the process's stderr forever and outputs it through OutputEvents.
--
debuggerThread :: LogAction IO Debugger.DebuggerLog
               -> HieBiosFlags    -- ^ GHC Invocation flags
               -> [String]        -- ^ Extra ghc args
               -> FilePath
               -> Debugger.RunDebuggerSettings -- ^ Settings for running the debugger
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> (DebugAdaptorCont () -> IO ())
               -- ^ Allows unlifting DebugAdaptor actions to IO. See 'registerNewDebugSession'.
               -> IO ()
debuggerThread l HieBiosFlags{..} extraGhcArgs mainFp runConf requests replies withAdaptor = do

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
        forever $ do
          req <- takeMVar requests & liftIO
          resp <- (Debugger.execute req <&> Right)
                    `catch` \(e :: SomeException) ->
                        pure (Left (displayExceptionWithContext e))
          either bad reply resp
    )
    [ Handler $ \(e::SomeAsyncException) -> do
        throwIO e
    ]

  where
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
  -> LogAction IO T.Text -- ^ Logger that writes to to DAP output
  -> Handle              -- ^ Handle to DAP output
  -> RunInTerminalProc
  -> IO (LogAction IO Debugger.DebuggerLog)
createDebuggerLogger l dapLogger writeDAPOutput runInTerminalProc = do
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
        Debugger.LogDebuggeeOut txt -> debuggeeOut dapLogger msyncProxyOut txt
        Debugger.LogDebuggeeErr txt -> debuggeeOut dapLogger msyncProxyErr txt
        _ -> pure () -- don't log other messages, already logged to (1)
        )
  where
    debuggeeOut l' mproxyChan txt = do
      -- (2)
      l' <& txt
      -- (3)
      case mproxyChan of
        Nothing -> pure ()
        Just proxyChan ->
          writeChan proxyChan $
            T.encodeUtf8 (txt <> "\n")

    (msyncProxyOut, msyncProxyErr)
      | RunProxyInTerminal{syncProxyOut, syncProxyErr} <- runInTerminalProc
      = (Just syncProxyOut, Just syncProxyErr)
      | otherwise
      = (Nothing, Nothing)

--------------------------------------------------------------------------------
-- * Capturing stdout, stderr, and writing to self stdin
--------------------------------------------------------------------------------

-- | Hijack the current process stdin and forward to it the messages from the given channel
stdinForwardThread :: Chan BS.ByteString -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdinForwardThread syncIn _withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stdin Forward Thread"

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
stdoutCaptureThread :: Maybe (Chan BS.ByteString) -> (DebugAdaptorCont () -> IO ()) -> IO ()
stdoutCaptureThread msyncOut withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stdout Capture Thread"
  withInterceptedStdout $ \_ interceptedStdout -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStdout
      case msyncOut of
        Nothing -> pure ()
        Just syncOut -> writeChan syncOut $ T.encodeUtf8 (line <> T.pack "\n")

      -- Always output to Debug Console
      catch
        (withAdaptor $ Output.stdout line)
        (\(_ :: IOException) ->
          throwIO (FailedToWriteToAdaptor line))

-- | Like 'stdoutCaptureThread' but for stderr
stderrCaptureThread :: Maybe (Chan BS.ByteString) -> (DebugAdaptorCont () -> IO ()) -> IO ()
stderrCaptureThread msyncErr withAdaptor = do
  tid <- myThreadId
  labelThread tid "Stderr Capture Thread"
  withInterceptedStderr $ \_ interceptedStderr -> do
    forever $ do
      line <- liftIO $ T.hGetLine interceptedStderr
      case msyncErr of
        Nothing -> pure ()
        Just syncErr -> writeChan syncErr $ T.encodeUtf8 (line <> "\n")

      -- Always output to Debug Console
      catch
        (withAdaptor $ Output.stderr line)
        (\(_ :: IOException) ->
          throwIO (FailedToWriteToAdaptor line))

newtype FailedToWriteToAdaptor = FailedToWriteToAdaptor T.Text
instance Show FailedToWriteToAdaptor where
  show (FailedToWriteToAdaptor t) = "Failed to write to debug adapter: " ++ T.unpack t
instance Exception FailedToWriteToAdaptor
