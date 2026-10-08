module GHC.Debugger.Backend where

import Colog.Core as Logger
import qualified GHC.Debugger as Debugger
import qualified GHC.Debugger.Monad as Debugger
import qualified GHC.Debugger.Interface.Messages as D (Command, Response)
import GHC.Debugger.Interface.Messages hiding (Command, Response)
import GHC
import GHC.Debugger.Debuggee as Debugger

import Control.Concurrent
import Control.Monad.IO.Class
import GHC.Conc
import GHC.Debugger.Utils (text)
import Control.Exception
import Control.Monad.Catch qualified as MC
import Data.Function


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
  liftIO $ do
    tid <- myThreadId
    labelThread tid "Main Debugger Thread (before runDebugger)"
  Debugger.runDebugger l debugRunner runConf $ do
    liftIO $ do
      tid <- myThreadId
      labelThread tid "Main Debugger Thread"
    let loop = do
          req <- takeMVar requests & liftIO
          -- TODO: remove once interpCmd stops calling `uninterruptibleMask`
          -- while waiting for a reply.
          resp <- Debugger.interruptible $ MC.try (Debugger.execute req)
          case resp of
            Right x -> do
              liftIO (putMVar replies x)
              loop
            Left e | Just (Debugger.NonFatalException {Debugger.userMessage = userm,
                            Debugger.debugMessage = dbgm}) <- fromException e  -> do
              Debugger.logSDoc Logger.Error (text userm)
              Debugger.logSDoc Logger.Debug (text dbgm)
              liftIO $ putMVar replies (NonFatalError userm)
            Left e ->
              -- don't loop in this case! just exit.
              liftIO $ putMVar replies (Aborted ("Aborted debugger thread: " ++ displayExceptionWithInfo e))
    loop

debuggerThreadBackend :: LogAction IO Debugger.DebuggerLog
               -> Debugger.DebugRunner Ghc ()
               -> Debugger.RunDebuggerSettings -- ^ Settings for running the debugger
               -> IO (DbgBackend,IO ())
debuggerThreadBackend l runner settings = do
  cmd <- newEmptyMVar
  res <- newEmptyMVar
  let backend = DbgBackend { syncRequests = putMVar cmd, syncResponses = takeMVar res }
  pure (backend, debuggerThread l runner settings cmd res)
