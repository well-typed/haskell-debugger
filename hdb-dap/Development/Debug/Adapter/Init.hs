{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NondecreasingIndentation #-}

-- | TODO: This module should be called Launch.
module Development.Debug.Adapter.Init
  ( module Development.Debug.Adapter.Init
  , DAPSessionLog(..)
  )
  where

#if !MIN_VERSION_ghc(9,15,0)
-- no longer needs to be imported from here in 9.15
import GHC.Conc.Sync (labelThread)
#endif

import GHC.IO.Handle
import qualified Data.Text as T
import qualified System.Process as P
import Control.Exception (displayExceptionWithInfo)
import Control.Monad.Except
import Control.Monad.Trans
import Data.Function
import Data.Functor
import Data.Maybe
import Data.UUID.V4 qualified as UUID
import System.IO
import GHC.IO.Encoding
import Control.Monad.Catch
import Control.Concurrent
import Data.Aeson as Aeson
import GHC.Generics
import Data.Functor.Contravariant

import Development.Debug.Adapter
import Colog.Core as Logger
import qualified Development.Debug.Adapter.Output as Output

import GHC (Ghc)
import GHC.Utils.Logger (defaultLogActionWithHandles)
import GHC.Debugger.Utils (forwardHandleToLogger,withOriginalCurrentDirectory)
import qualified GHC.Debugger as Debugger
import qualified GHC.Debugger.Monad as Debugger
import qualified GHC.Debugger.Interface.Messages as D (Command, Response)
import GHC.Debugger.Interface.Messages hiding (Command, Response)

import DAP
import Development.Debug.Adapter.Handles
import Development.Debug.Session.Setup
import GHC.Debugger.Monad (RunDebuggerSettings(..))
import GHC.Debugger.Debuggee as Debugger
import Development.Debug.Adapter.DAPDebuggee

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
  , cradleFile :: Maybe FilePath
    -- ^ specify cradle file rather than let it be inferred from @entryFile@, relative to @projectRoot@.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON

--------------------------------------------------------------------------------
-- * Launch Debugger
--------------------------------------------------------------------------------

data DAPServerConf = DAPServerConf
  { hdbProgram :: FilePath
    -- ^ invoked with `external-interpreter` to serve as the external interpreter
  , getDebugRunner :: DebugRunnerProvider ()
  , dapServerConfig :: ServerConfig
  }

data InterpreterChoice = InterpreterChoice { runInTerminal :: Bool, internal :: Bool }


-- | Initialize debugger
--
-- Returns @()@ if successful, throws @InitFailed@ otherwise
initDebugger :: LogAction IO DAPSessionLog -> DAPServerConf -> InterpreterChoice
             -> LaunchArgs -> DebugAdaptor ()
initDebugger l servConf interpChoice
               LaunchArgs{ __sessionId
                         , projectRoot = givenRoot
                         , entryFile = entryFileMaybe
                         , entryPoint = fromMaybe "main" -> entryPoint
                         , entryArgs  = fromMaybe [] -> entryArgs
                         , extraGhcArgs = fromMaybe [] -> extraGhcArgs
                         , cradleFile
                         } = do
  syncRequests  <- liftIO newEmptyMVar
  syncResponses <- liftIO newEmptyMVar

  entryFile <- case entryFileMaybe of
    Nothing -> throwError ("Missing \"entryFile\" key in debugger configuration", Nothing)
    Just ef -> pure ef

  -- | See Note [ Current directory is a global property that affects HIE and GHC ]
  projectRoot <- liftIO $ withOriginalCurrentDirectory
    $ \ (mkAbsolute -> cwd) -> pure $ maybe cwd (cwd />) givenRoot

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

  let debugRunnerConf = DebugRunnerConf (unAbs projectRoot) entryFile extraGhcArgs cradleFile

  liftIO (getDebugRunner servConf hieBiosLogger debugRunnerConf) >>= \case
    Left e              -> throwError (ErrorMessage (T.pack e), Nothing)
    Right (ghcInvocation, debugRunner) -> do

      let
        nextFreshId = 0
        breakpointMap = mempty
        stackFrameMap = mempty
        variablesMap  = mempty

      dbgLog <- liftIO $ createDebuggerLogger l dapLogger writeDAPOutput

      dapd <- initDAPDebuggee l (hdbProgram servConf) interpChoice

      let
        defaultRunConf = Debugger.RunDebuggerSettings
          { supportsANSIStyling = True     -- TODO: Initialize Request sends supportsANSIStyling; this is False for nvim-dap
          , supportsANSIHyperlinks = False -- VSCode does not support this
          , interpreterSettings = dapdInterpreterSettings dapd
          }
        absEntryFile = projectRoot /> entryFile
        daState = DAS{entryFile=absEntryFile,waitForDebuggee = dapdWaitForDebuggee dapd,..}

      -- TODO: is nextRandom threadsafe?
      sessionId <- liftIO $ maybe (("debug-session:" <>) . T.show <$> UUID.nextRandom) (pure . T.pack) __sessionId

      registerNewDebugSession sessionId daState $
        [ \withAdaptor -> do
            -- The info here is already taken into account in debugRunner.
            let GhcInvocation libdir units args = ghcInvocation
            withAdaptor $
              Output.console $ T.pack $ unlines $
                [ "libdir: " <> libdir
                , "units: " <> unwords units
                , "args: " <> unwords args
                ]
            debuggerThread dbgLog debugRunner defaultRunConf syncRequests syncResponses
        , \withAdaptor -> forwardHandleToLogger readDAPOutput $
            LogAction (\msg -> withAdaptor (Output.neutral msg))
        ]
        ++
        dapdThreads dapd

      dapdAfterRegister dapd

initDAPDebuggee
  :: LogAction IO DAPSessionLog
  -> FilePath
  -> InterpreterChoice
  -> DebugAdaptor DAPDebuggee
initDAPDebuggee _ _ InterpreterChoice{runInTerminal = False, internal = True}
  = internalNoInTerminalDAPD
initDAPDebuggee _ hdbProg InterpreterChoice{runInTerminal = False, internal = False}
  = externalNoInTerminalDAPD hdbProg
initDAPDebuggee _ hdbProg InterpreterChoice{internal = False, runInTerminal = True}
  = externalInTerminalDAPD hdbProg
initDAPDebuggee l hdbProg InterpreterChoice{runInTerminal = True, internal = True}
  = internalInTerminalDAPD l hdbProg


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
               -> Debugger.DebugRunner Ghc ()
               -> Debugger.RunDebuggerSettings -- ^ Settings for running the debugger
               -> MVar D.Command  -- ^ Read commands
               -> MVar D.Response -- ^ Write reponses
               -> IO ()
debuggerThread l debugRunner runConf requests replies = do
  Debugger.runDebugger l debugRunner runConf $ do
    liftIO $ do
      tid <- myThreadId
      labelThread tid "Main Debugger Thread"
    let loop = do
          req <- takeMVar requests & liftIO
          resp <- (Debugger.execute req <&> Right)
                    `catch` \(e :: SomeException) -> do
                        pure (Left (displayExceptionWithInfo e))
          case resp of
            Right x -> do
              liftIO (putMVar replies x)
              loop
            Left m ->
              -- don't loop in this case! just exit.
              liftIO $ putMVar replies (Aborted ("Aborted debugger thread: " ++ m))
    loop

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
  :: LogAction IO DAPSessionLog
  -> LogAction IO T.Text -- ^ Logger that writes to to DAP output
  -> Handle              -- ^ Handle to DAP output
  -> IO (LogAction IO Debugger.DebuggerLog)
createDebuggerLogger l dapLogger writeDAPOutput = do
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
        Debugger.LogDebuggeeOut txt -> dapLogger <& txt
        Debugger.LogDebuggeeErr txt -> dapLogger <& txt
        _ -> pure () -- don't log other messages, already logged to (1)
        )
