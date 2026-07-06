{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, CPP, DeriveAnyClass,
   DeriveGeneric, DerivingVia, LambdaCase, RecordWildCards, ViewPatterns,
   DataKinds #-}
module Main where

import System.Process
import System.Environment
import Control.Exception (bracket, uninterruptibleMask, bracketOnError)
import Control.Exception.Backtrace

import DAP

import Development.Debug.Adapter.Init
import Development.Debug.Adapter.Handles
import Development.Debug.Adapter.Server
import Colog.Core

import System.IO
  ( hFlush
  , hClose
  , hPutStrLn
  , hSetBuffering
  , BufferMode(..)
  , Handle

  , IOMode(ReadWriteMode)
  )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD
import Data.Functor.Contravariant
import Network.Socket hiding (Debug)

import qualified GHCi.Server as GHCi
import qualified GHCi.Signals as GHCi
import qualified GHCi.Utils as GHCi
import qualified GHCi.Message as GHCi

import GHC.Debugger.Monad (RunDebuggerSettings(..))
import Development.Debug.Options (HdbOptions(..))
import Development.Debug.Options.Parser (parseHdbOptions)
import Development.Debug.Adapter.Proxy
import Development.Debug.Interactive
import GHC.Stack.Annotation (annotateCallStackIO)
import GHC.Utils.Logger (defaultLogActionWithHandles)
import Development.Debug.Session.Setup (hieDebugRunner)
import GHC.Debugger.Debuggee (mkCliInterpreterSettings)

#if MIN_VERSION_ghc(9,15,0)
import GHC.Debugger.Runtime.Interpreter.Custom (dbgInterpCmdHandler)
#endif

--------------------------------------------------------------------------------

main :: IO ()
main = do
  setBacktraceMechanismState CostCentreBacktrace False
  setBacktraceMechanismState HasCallStackBacktrace True

  allArgs <- getArgs
  hdbOpts <- case allArgs of
    [writeFd, readFd, "--external-interpreter"] ->
         -- Special case to detect --external-interpreter in the third
         -- position. If we could specify -opti options to put *before* the
         -- descriptors we could get rid of this.
         pure (HdbExternalInterpreter (read writeFd) (read readFd))
    _ -> parseHdbOptions
  case hdbOpts of
    HdbDAPServer{port, internalInterpreter, disableIpeBacktraces} -> do
      setBacktraceMechanismState IPEBacktrace (not disableIpeBacktraces)
      config <- getConfig port
      -- the same program invoked with `external-interpreter` serves as the external interpreter
      hdbProgram <- getExecutablePath
      let servConf = DAPServerConf
            { getDebugRunner = hieDebugRunner
            , hdbProgram
            , dapServerConfig = config
            }
      redirectRealStdout internalInterpreter $ \realStdout -> do
        hSetBuffering realStdout LineBuffering
        l <- contramap DAPLog <$> mainLogger hdbOpts.verbosity realStdout
        runDAPServerWithLogger (contramap DAPLibraryLog l) config
          (talk l servConf internalInterpreter)
          (ack l )
    HdbCLI{..} -> do
        setBacktraceMechanismState IPEBacktrace (not disableIpeBacktraces)
        l <- mainLogger hdbOpts.verbosity stdout
        cliInterpSettings <- mkCliInterpreterSettings internalInterpreter debuggeeStdin
        let runConf = RunDebuggerSettings
              { supportsANSIStyling = True -- todo: check!!
              , supportsANSIHyperlinks = False
              , interpreterSettings = cliInterpSettings }
        runIDM (contramap InteractiveLog l) entryPoint entryFile entryArgs extraGhcArgs cradleFile
          runConf debugInteractive
    HdbProxy{port} -> do
        setBacktraceMechanismState IPEBacktrace True
        l <- mainLogger hdbOpts.verbosity stdout
        runInTerminalHdbProxy (contramap RunProxyClientLog l) port
    HdbExternalInterpreter{writeFd, readFd} -> do
      inh  <- GHCi.readGhcHandle (show readFd)
      outh <- GHCi.readGhcHandle (show writeFd)
      runExternalInterpreterServer inh outh
    HdbExternalInterpreterPort{port} -> do
      pid <- getCurrentPid
      withExternalInterpreterPort (fromIntegral port) $ \h -> do
        hPutStrLn h (show pid)
        hFlush h
        runExternalInterpreterServer h h
  where
    runExternalInterpreterServer inh outh = do
      GHCi.installSignalHandlers
      pipe <- GHCi.mkPipeFromHandles inh outh
      let verbose = False
#if MIN_VERSION_ghc(9,15,0)
      uninterruptibleMask $ \restore ->
        GHCi.servWithCustom verbose hook pipe restore dbgInterpCmdHandler
#else
      uninterruptibleMask $ GHCi.serv verbose hook pipe
#endif
      where hook = return -- empty hook
        -- we cannot allow any async exceptions while communicating, because
        -- we will lose sync in the protocol, hence uninterruptibleMask.

    withExternalInterpreterPort :: PortNumber -> (Handle -> IO a) -> IO a
    withExternalInterpreterPort port k = do
      bracket (mkHandleFromPortSock "127.0.0.1" port) hClose $ \ h -> do
        annotateCallStackIO $ k h

    mkHandleFromPortSock :: HostName -> PortNumber -> IO Handle
    mkHandleFromPortSock host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just (show port))

      Control.Exception.bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (\sock -> do
            -- Don't delay, avoids batching
            setSocketOption sock NoDelay 1
            connect sock (addrAddress addr)
            h <- socketToHandle sock ReadWriteMode
            hSetBuffering h NoBuffering
            return h)

    -- When using the internal interpreter in DAP mode, we can't write to
    -- stdout directly because there will also be a thread forwarding the
    -- debuggee stdout by capturing it from stdout (and we'd get into a loop
    -- trying to forward what we're writing).
    --
    -- The redirection we use requires hDuplicateTo which isn't supported on
    -- Windows (ghc#22146), so using the internal interpreter on Windows
    -- currently unsupported.
    --
    -- When using the external interpreter, the debuggee output is read from
    -- its process handle directly, so this is unnecessary.
    redirectRealStdout internalInterpreter k
      | internalInterpreter =
        withInterceptedStdoutForwarding
          (\interceptedOut -> T.hPutStrLn stderr ("[INTERCEPTED STDOUT] " <> interceptedOut))
          (\realStdout -> k realStdout)
      | otherwise = k stdout



--------------------------------------------------------------------------------
-- * Logging
--------------------------------------------------------------------------------

data MainLog
  = InteractiveLog InteractiveLog
  | RunProxyClientLog (WithSeverity T.Text)
  | DAPLog DAPLog

-- | Given the severity threshold from which we start logging, create a base
-- logger for consuming the top-level debugger logs ('MainLog').
-- Outputs to given handle.
mainLogger :: Severity -> Handle -> IO (LogAction IO MainLog)
mainLogger threshold h = do
  l <- handleLogger h
  let logGhcLog = defaultLogActionWithHandles h h
  pure $ LogAction $ \case
    InteractiveLog (ISessionSetupLog sessionLog) -> logSessionLog l threshold sessionLog
    InteractiveLog (IDebuggerLog debuggerLog)    -> logDebuggerLog logGhcLog l threshold debuggerLog
    RunProxyClientLog sev_msg -> defaultLog l threshold sev_msg
    DAPLog dapLog -> logDAPLog logGhcLog l threshold <& dapLog
