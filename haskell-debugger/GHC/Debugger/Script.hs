{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.Coerce
import Control.Monad
import qualified Data.List as List

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
  , projectRoot :: AbsFilePath
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

-- | Result of setting a breakpoint
-- TODO: unpack into API types.
newtype BreakResult = BreakResult {unBR :: BreakFound }

-- | Somewhere we stopped at.
data Break = Break { breakId :: Maybe GHC.InternalBreakpointId, breakThread :: RemoteThreadId }

type LineNo = Int
type ColNo = Int

-- | Live thread (or thread id?), might be stopped or running.
newtype Thread = Thread DebuggeeThread -- TODO

newtype BackTrace = Backtrace [DbgStackFrame] -- TODO

data Value = Value
  { val :: String
  , type_ :: String
  , sourceKind :: Maybe SourceKind
  , structureRef :: VariableReference
  }

data DebuggeeException = DebuggeeException { val :: String, type_ :: String}

-- | Result of `run`.
data Exec = Stopped Break | ExceptionThrown DebuggeeException | Completed Value

data StackFrame = StackFrame {tId :: RemoteThreadId, ix :: Int, frame :: DbgStackFrame }

-- | Resolved variable in a StackFrame/Scope.
newtype Var = Var {unVar :: VarInfo}

-- | Returned by non-blocking calls, can wait on it.
data Async a = Async (Script a) -- TODO: not be supported by GHC.Debugger atm, we stash just the "handling of result" part in here.

data StackFrameIndexOutOfBound = StackFrameIndexOutOfBound Int
  deriving Show
instance Exception StackFrameIndexOutOfBound

-------------------------------------------------------------
-- Internals
-------------------------------------------------------------

handleEvalResult :: EvalResult -> Script Exec
handleEvalResult res = do
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
      setCurrentState NotStopped
      liftIO $ throw $ DebuggeeEvalAborted s

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

getCurrentStackPos :: Script (RemoteThreadId,Int)
getCurrentStackPos = do
  CurrentlyStopped{..} <- getCurrentState
  pure (_break.breakThread,frameIx)


setCurrentStackPos :: (RemoteThreadId, Int) -> Script ()
setCurrentStackPos (tId,frameIx) = do
  CurrentlyStopped{frameIx =_, _break = Break{..}} <- getCurrentState
  unless (breakThread == tId) $ do
    error $ "setCurrentStackPos: TBD breakThread /= tId"
  setCurrentState $ CurrentlyStopped{_break = Break {breakThread = tId, breakId},frameIx}


-------------------------------------------------------
-- API
-------------------------------------------------------

break :: FilePath -> LineNo -> Maybe ColNo -> Script BreakResult
break fp lineNum columnNum = do
  env <- ask
  let path = env.debugExecutionArgs.projectRoot /> fp
  DidSetBreakpoint bf <- sendSync $ SetBreakpoint (ModuleBreak{..}) Nothing Nothing Nothing
  return $ BreakResult bf

-- the "active thread" is implicitly set to the returned Break's thread
run :: Script Exec
run = do
  DebugExecutionArgs{..} <- asks debugExecutionArgs
  DidExec res <- sendSync DebugExecution{..}
  handleEvalResult res


-- Nothing = active thead
backtrace :: Maybe Thread -> Script BackTrace
backtrace m =
  case m of
    Nothing -> do
      CurrentlyStopped{..} <- getCurrentState
      backtrace' (_break.breakThread)
    Just (Thread t) -> do
      backtrace' t.tId
  where
    backtrace' :: RemoteThreadId -> Script BackTrace
    backtrace' tid = do
      GotStacktrace st <- sendSync $ GetStacktrace tid
      pure $ Backtrace st

threads :: Script [Thread]
threads = do
  GotThreads ts <- sendSync GetThreads
  pure $ coerce ts


-- make the "current stack frame" the frame 5 frames before the current one for the current active thread
up, down :: Int -> Script StackFrame
up n = do
  (tId,frameIx) <- getCurrentStackPos
  Backtrace st <- backtrace (Just (Thread (DebuggeeThread tId Nothing)))
  let newFrameIx = frameIx + n
  case st List.!? newFrameIx of
    Just fr -> do
      setCurrentStackPos (tId,newFrameIx)
      return $ StackFrame tId newFrameIx fr
    Nothing -> do
      liftIO $ throw (StackFrameIndexOutOfBound newFrameIx)
down = up . negate

-- Nothing = current stack frame.
var :: Maybe StackFrame -> String -> Script (Maybe Var)
var m name0 = do
  case m of
    Nothing -> do
      (tId,ix) <- getCurrentStackPos
      displayVar' tId ix name0
    Just StackFrame{tId,ix} -> do
      displayVar' tId ix name0
  where
    displayVar' tId ix name = do
      GotVariables vs <- sendSync $ GetVariables tId ix LocalVariables
      pure . fmap Var $ List.find ((== name) . varName) (variableResultToList vs)

-- Non-blocking, sets per-thread.
-- TODO: actually non-block
resume :: Thread -> Script (Async Exec)
resume (Thread dbgTh) = do
  DidResume e <- sendSync $ DoResume dbgTh.tId ResumeNoStep ResumeJustThisThread
  pure $ Async $ handleEvalResult e


-- TODO: Async TBD
waitAny :: [Async a] -> Script a
waitAny (Async x:_) = x
waitAny [] = error "waitAny: Nothing to wait on."

-- resume all, set stop-the-world, wait until break
resumeAll :: Script Exec
resumeAll = do
  (tId,_) <- getCurrentStackPos
  DidResume e <- sendSync $ DoResume tId ResumeNoStep ResumeTheWorld
  handleEvalResult e


