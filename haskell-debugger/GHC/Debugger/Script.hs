{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DefaultSignatures #-}
module GHC.Debugger.Script where

import GHC.Debugger.Interface.Messages
import qualified Data.Text as T
import Control.Monad.IO.Class

import GHC.Debugger.Interface.Messages as D
import Data.Text
import Control.Exception
import Data.IORef
import qualified GHC
import Data.Coerce
import Control.Monad
import qualified Data.List as List
import Control.Monad.Catch as MC

-------------------------------------------
-- Types
-------------------------------------------
data ScriptMsg = ErrorMessage ErrorMessage | Stdout Text
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
  { dbgBackend         :: DbgBackend
  , _output            :: ScriptMsg -> IO ()
  , currentStop        :: IORef LocalState
  , debugExecutionArgs :: DebugExecutionArgs
  }

newtype Script a = Script {unScript :: ScriptEvalEnv -> IO a}

instance Functor Script where
  fmap f (Script m) = Script $ fmap f . m

instance Applicative Script where
  pure x = Script $ const (pure x)
  Script f <*> Script m = Script $ \ x -> f x <*> m x
instance Monad Script where
  Script m >>= f = Script $ \x -> m x >>= \ a -> unScript (f a) x
instance MonadIO Script where
  liftIO m = Script $ const m
instance MonadFail Script where
  fail s = liftIO $ fail s
instance MonadThrow Script where
  throwM e = liftIO $ throwM e
instance MonadCatch Script where
  catch (Script m) h = Script $ \ x -> MC.catch (m x) $ \ e -> unScript (h e) x
instance MonadMask Script where
  mask f = Script $ \ x -> MC.mask $ \ restore -> unScript (f $ Script . (restore .) . unScript) x
  uninterruptibleMask f = Script $ \ x -> MC.uninterruptibleMask $ \ restore -> unScript (f $ Script . (restore .) . unScript) x
  generalBracket (Script o) c k = Script $ \ x -> generalBracket (o x) (\ a e -> unScript (c a e) x) (\ a -> unScript (k a) x)

runScript :: ScriptEvalEnv -> Script a -> IO a
runScript e (Script m) = m e

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
  deriving Show

instance REPLShow BreakResult

-- | Somewhere we stopped at.
data Break = Break { breakId :: Maybe GHC.InternalBreakpointId, breakThread :: RemoteThreadId }
  deriving Show

instance REPLShow Break

type LineNo = Int
type ColNo = Int

-- | Live thread (or thread id?), might be stopped or running.
newtype Thread = Thread DebuggeeThread -- TODO
  deriving Show

instance REPLShow Thread

newtype BackTrace = Backtrace [DbgStackFrame] -- TODO
  deriving Show

instance REPLShow BackTrace

data Value = Value
  { val :: String
  , type_ :: String
  , sourceKind :: Maybe SourceKind
  , structureRef :: VariableReference
  }

instance Show Value where
  show v = v.val ++ " :: " ++ v.type_

instance REPLShow Value

data DebuggeeException = DebuggeeException { val :: String, type_ :: String}
  deriving Show

instance REPLShow DebuggeeException

-- | Result of `run`.
data Exec = Stopped Break | ExceptionThrown DebuggeeException | Completed Value
  deriving Show

instance REPLShow Exec

data StackFrame = StackFrame {tId :: RemoteThreadId, ix :: Int, frame :: DbgStackFrame }
  deriving Show

instance REPLShow StackFrame

-- | Resolved variable in a StackFrame/Scope.
newtype Var = Var {unVar :: VarInfo}

-- | Returned by non-blocking calls, can wait on it.
data Async a = Async (Script a) -- TODO: not be supported by GHC.Debugger atm, we stash just the "handling of result" part in here.


data StackFrameIndexOutOfBound = StackFrameIndexOutOfBound Int
  deriving Show

instance Exception StackFrameIndexOutOfBound

class REPLShow a where
  replShow :: a -> T.Text
  default replShow :: Show a => a -> T.Text
  replShow = Data.Text.show

replPrint :: REPLShow a => a -> Script ()
replPrint x = do
  l <- asks' _output
  liftIO $ l $ Stdout (replShow x)

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
setCurrentState s = liftIO . flip writeIORef s =<< asks' currentStop

getCurrentState :: Script LocalState
getCurrentState = liftIO . readIORef =<< asks' currentStop

asks' :: (ScriptEvalEnv -> a) -> Script a
asks' f = Script $ pure . f

ask' :: Script ScriptEvalEnv
ask' = asks' id

output :: ScriptMsg -> Script ()
output msg = do
  l <- asks' _output
  liftIO $ l msg

reportError :: ErrorMessage -> Script ()
reportError = output . ErrorMessage

-- | Synchronously send a command to the debugger and await a response
sendSync :: D.Command -> Script Response
sendSync cmd = do
  SEE{dbgBackend = DbgBackend{..}} <- ask'
  liftIO $ syncRequests cmd
  liftIO syncResponses >>= handleErrors

-- | Sends a command to the debugger, then runs the given action, and only after
-- running the action it waits for the result of the debugger
sendInterleaved :: D.Command -> Script () -> Script Response
sendInterleaved cmd action = do
  SEE{dbgBackend = DbgBackend{..}} <- ask'
  liftIO $ syncRequests cmd
  () <- action
  liftIO syncResponses >>= handleErrors

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
  env <- ask'
  let path = env.debugExecutionArgs.projectRoot /> fp
  DidSetBreakpoint bf <- sendSync $ SetBreakpoint (ModuleBreak{..}) Nothing Nothing Nothing
  return $ BreakResult bf

-- the "active thread" is implicitly set to the returned Break's thread
run :: Script Exec
run = do
  DebugExecutionArgs{..} <- asks' debugExecutionArgs
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


