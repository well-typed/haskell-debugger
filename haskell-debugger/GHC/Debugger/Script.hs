{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module GHC.Debugger.Script where

import GHC.Debugger.Interface.Messages


import qualified Data.Text as T
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import GHC.Debugger.Interface.Messages as D
import Control.Monad.Reader
import Data.Text
import Colog.Core
import Control.Exception
import Data.IORef
import qualified GHC

-------------------------------------------
-- Types
-------------------------------------------
data ScriptMsg = ErrorMessage ErrorMessage
  deriving Show
type ErrorMessage = Text

data LocalState
  = CurrentlyStopped
    { _break :: Break
    , frameIx  :: Int
    }

  | NotStopped

data DebugExecutionArgs = DebugExecutionArgs
  { entryPoint :: EntryPoint
  , entryFile  :: AbsFilePath
  , runArgs    :: [String]
  }

data ScriptEvalEnv = SEE
  { syncRequests       :: MVar D.Command
  , syncResponses      :: MVar D.Response
  , _output            :: LogAction IO ScriptMsg
  , currentStop        :: IORef LocalState
  , debugExecutionArgs :: DebugExecutionArgs
  }

newtype Script a = Script (ReaderT ScriptEvalEnv IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader ScriptEvalEnv)

data AbortedDebugSession = AbortedDebugSession T.Text
  deriving Show

instance Exception AbortedDebugSession

data DebuggeeEvalAborted = DebuggeeEvalAborted String
  deriving Show

instance Exception DebuggeeEvalAborted

----------------------------------------------
-- API Types
----------------------------------------------

-- | Somewhere we stopped at.
data Break = Break { breakId :: Maybe GHC.InternalBreakpointId, breakThread :: RemoteThreadId }

type LineNo = Int
type ColNo = Int

-- | Live thread (or thread id?), might be stopped or running.
data Thread -- TODO

data BackTrace -- TODO

data Value = Value
  { val :: String
  , type_ :: String
  , sourceKind :: Maybe SourceKind
  , structureRef :: VariableReference
  }

data DebuggeeException = DebuggeeException { val :: String, type_ :: String}

-- | Result of `run`.
data Exec = Stopped Break | ExceptionThrown DebuggeeException | Completed Value

data StackFrame -- TODO

-- | Naming a variable in a StackFrame/Scope.
data Var -- TODO

-- | Returned by non-blocking calls, can wait on it.
data Async a -- TODO: might not be supported by GHC.Debugger atm.

-------------------------------------------------------------
-- Internals
-------------------------------------------------------------

setCurrentState :: LocalState -> Script ()
setCurrentState s = liftIO . flip writeIORef s =<< asks currentStop

getCurrentState :: Script LocalState
getCurrentState = liftIO . readIORef =<< asks currentStop

output :: ScriptMsg -> Script ()
output msg = do
  l <- asks _output
  liftIO $ l <& msg

reportError :: ErrorMessage -> Script ()
reportError = output . ErrorMessage

-- | Synchronously send a command to the debugger and await a response
sendSync :: D.Command -> Script Response
sendSync cmd = do
  SEE{..} <- ask
  liftIO $ putMVar syncRequests cmd
  liftIO (takeMVar syncResponses) >>= handleErrors

-- | Sends a command to the debugger, then runs the given action, and only after
-- running the action it waits for the result of the debugger
sendInterleaved :: D.Command -> Script () -> Script Response
sendInterleaved cmd action = do
  SEE{..} <- ask
  liftIO $ putMVar syncRequests cmd
  () <- action
  liftIO (takeMVar syncResponses) >>= handleErrors

handleErrors :: Response -> Script Response
handleErrors r@(NonFatalError e) = do
  reportError (T.pack e)
  pure r
handleErrors (Aborted e) = do
  let s = T.pack e
  reportError s
  liftIO $ throw (AbortedDebugSession s)
handleErrors r = return r

-------------------------------------------------------
-- API
-------------------------------------------------------

break :: FilePath -> LineNo -> Maybe ColNo -> Script ()
break = undefined

-- the "active thread" is implicitly set to the returned Break's thread
run :: Script Exec
run = do
  DebugExecutionArgs{..} <- asks debugExecutionArgs
  DidExec res <- sendSync DebugExecution{..}
  case res of
    EvalCompleted{..} -> do
      setCurrentState NotStopped
      return $ Completed $ Value
        { val = resultVal
        , type_ = resultType
        , sourceKind = resultSourceKind
        , structureRef = resultStructureRef
        }
    EvalException{..} -> do
      setCurrentState NotStopped
      return $ ExceptionThrown (DebuggeeException resultVal resultType)
    EvalStopped{..} -> do
      let b = Break{..}
      setCurrentState $ CurrentlyStopped{ _break = b, frameIx = 0}
      pure $ Stopped b
    EvalAbortedWith s -> do
      liftIO $ throw $ DebuggeeEvalAborted s


-- Nothing = active thead
backtrace :: Maybe Thread -> Script BackTrace
backtrace = undefined

threads :: Script [Thread]
threads = undefined

-- make the "current stack frame" the frame 5 frames before the current one for the current active thread
up, down :: Int -> Script ()
up = undefined
down = undefined

-- Nothing = current stack frame.
displayVar :: Maybe StackFrame -> Var -> Script T.Text
displayVar = undefined

-- Non-blocking, sets per-thread.
resume :: Thread -> Script (Async Break)
resume = undefined

waitAny :: [Async a] -> Script a
waitAny = undefined

-- resume all, set stop-the-world, wait until break
resumeAll :: Script Break
resumeAll = undefined

