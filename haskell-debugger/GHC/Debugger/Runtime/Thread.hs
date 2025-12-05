{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- TODO
-- - [] Consider caching once and forall the expressions we dynamically compile and load in this module.
module GHC.Debugger.Runtime.Thread
  ( getRemoteThreadIdFromRemoteContext
  , getRemoteThreadId
  , getRemoteThreadsLabels
  , getRemoteThreadStackCopy
  , getRemoteThreadIPEStack
  , listAllLiveRemoteThreads
  ) where

import Data.Maybe
import Data.Functor
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import GHC.Conc.Sync
import GHC.Stack.CloneStack
import GHC.Exts.Heap.ClosureTypes

import GHC
import GHC.Builtin.Types
import GHC.Runtime.Heap.Inspect

import GHC.Driver.Config
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp
import GHC.Utils.Outputable

import GHCi.Message
import GHCi.RemoteTypes

import GHC.Debugger.Utils
import GHC.Debugger.Logger as Logger
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Term.Parser
import GHC.Debugger.Runtime.Thread.Map
import GHC.Debugger.Runtime.Eval

-- | Get a 'RemoteThreadId' from a remote 'ResumeContext' gotten from an 'ExecBreak'
getRemoteThreadIdFromRemoteContext :: ForeignRef (ResumeContext [HValueRef]) -> Debugger RemoteThreadId
getRemoteThreadIdFromRemoteContext fctxt = do
  -- Get the ResumeContext term and fetch the resumeContextThreadId field
  parsed_threadid <- obtainParsedTerm "RemoteContext's ThreadId" 2 True anyTy (castForeignRef fctxt)
                        (subtermWith 2{-RemoteContext's ThreadId-} anyTerm)
  case parsed_threadid of
    Left errs -> do
      logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
      liftIO $ fail "Failed to parse remote ResumeContext's thread id"
    Right Term{val=threadIdVal} -> do
      getRemoteThreadId (castForeignRef threadIdVal)
    _ -> liftIO $ fail "Expected threadIdTerm to be a Term!"

getRemoteThreadId :: ForeignRef ThreadId -> Debugger RemoteThreadId
getRemoteThreadId threadIdRef = do
  from_thread_id_fv <- compileExprRemote "GHC.Conc.Sync.fromThreadId"
  thread_id_fv      <- expectRight =<<
                       evalApplication from_thread_id_fv (castForeignRef threadIdRef)
  parsed_int <-
    obtainParsedTerm "ThreadId's Int value" 2 True wordTy{-really, Word64, but we won't look at the type-} thread_id_fv intParser

  case parsed_int of
    Left errs -> do
      logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
      liftIO $ fail "Failed to parse remote thread id on fromThreadId result!"
    Right tid_int -> do

      tmap_ref <- asks threadMap
      -- unconditionally write to the map the foreign ref (it should always
      -- refer to the same ThreadId as a possible existing entry)
      liftIO $ modifyIORef' tmap_ref $
        insertThreadMap tid_int threadIdRef

      return (RemoteThreadId tid_int)

-- | Is the remote thread running or blocked (NOT finished NOR dead)?
getRemoteThreadStatus :: ForeignRef ThreadId -> Debugger ThreadStatus
getRemoteThreadStatus threadIdRef = do
  thread_status_fv <- compileExprRemote "GHC.Conc.Sync.threadStatus"
  status_fv        <- expectRight =<<
                      evalApplicationIO thread_status_fv (castForeignRef threadIdRef)
  status_parsed    <- obtainParsedTerm "ThreadStatus" 2 True anyTy{-..no..-} status_fv threadStatusParser

  case status_parsed of
    Left errs -> do
      logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
      liftIO $ fail "Failed to parse ThreadStatus"
    Right thrdStatus ->
      return thrdStatus

isRemoteThreadLive :: ForeignRef ThreadId -> Debugger Bool
isRemoteThreadLive r = getRemoteThreadStatus r <&> \case
  (ThreadRunning ; ThreadBlocked{}) -> True
  (ThreadDied    ; ThreadFinished)  -> False

-- | Call 'listThreads' on the (possibly) remote debuggee process to get the
-- list of threads running on the debuggee. Filter by running threads
-- This may include the debugger threads if using the internal interpreter.
listAllLiveRemoteThreads :: Debugger [(RemoteThreadId, ForeignRef ThreadId)]
listAllLiveRemoteThreads = do
  hsc_env <- getSession

  list_threads_fv <- compileExprRemote "GHC.Conc.Sync.listThreads"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp    = hscInterp hsc_env

  threads_fvs <- expectRight =<< handleMultiStatus <$>
                 liftIO (evalStmt interp eval_opts (EvalThis list_threads_fv))
  catMaybes <$> do
    forM threads_fvs $ \(castForeignRef -> thread_fv) -> do
      isLive <- isRemoteThreadLive thread_fv
      if isLive then do
        tid <- getRemoteThreadId thread_fv
        pure $ Just (tid, thread_fv)
      else do
        pure Nothing

-- | Get the label of a Thread in a remote process. Returns one element per
-- given Thread with @Just string@ if a label was found.
getRemoteThreadsLabels :: [ForeignRef ThreadId] -> Debugger [Maybe String]
getRemoteThreadsLabels threadIdRefs = do
  hsc_env <- getSession

  -- evalStmt takes an IO [a] and evals it into a [ForeignHValue]. Represent the Maybe as an empty list
  thread_label_fv <- compileExprRemote "(fmap (\\ms -> case ms of Nothing -> []; Just s -> [pure s]) \
                                           . GHC.Conc.Sync.threadLabel \
                                            ) :: GHC.Conc.Sync.ThreadId -> IO [IO String]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp    = hscInterp hsc_env

  forM threadIdRefs $ \threadIdRef ->
    handleMultiStatus <$>
    liftIO (evalStmt interp eval_opts (EvalApp (EvalThis thread_label_fv) (EvalThis (castForeignRef threadIdRef))))
      >>= expectRight
      >>= \case
        []          -> pure Nothing
        [io_lbl_fv] -> Just <$> liftIO (evalString interp io_lbl_fv)
        _ -> liftIO $ fail "Unexpected result from evaluating \"threadLabel\""

-- | Clone the stack of the given remote thread and get the Term
getRemoteThreadStackCopy :: ForeignRef ThreadId -> Debugger Term
getRemoteThreadStackCopy threadIdRef = do
  hsc_env <- getSession

  thread_stack_fv <- compileExprRemote "GHC.Exts.Stack.decodeStack Control.Monad.<=< GHC.Stack.CloneStack.cloneThreadStack"

  -- TODO: Currently, GHC.Stack.CloneStack.decode, which uses the IPE
  -- information to report source locations of the callstacks, does not work
  -- for a stack with interpreter return frames. We would probably also like to
  -- use that.

  evalApplicationIO thread_stack_fv (castForeignRef threadIdRef)
    >>= \case
      Left (EvalRaisedException e) -> do
        logSDoc Logger.Info (text "Failed to decode the stack with" <+> text (show e) $$ text "This is likely bug #26640 in the decoder, which has been fixed for 9.14.2 and forward. No StackTrace will be returned...")
        return []
      Left e -> do
        logSDoc Logger.Warning (text "Failed to decode the stack with" <+> text (show e) $$ text "No StackTrace will be returned...")
        return []
      Right stack_frames_fv ->
        liftIO $ cvObtainTerm hsc_env 2 True anyTy{-todo:stackframety?-} stack_frames_fv

-- | Try to get an IPE stacktrace.
--
-- At the moment, we assume IPE stacktraces are always empty @[]@ for threads
-- with interpreter frames.
--
-- Really, as long as there is IPE information, this function should return
-- StackEntries for all frames, including the interpreter ones, since these are
-- typically be interleaved with "normal" frames.
getRemoteThreadIPEStack :: ForeignRef ThreadId -> Debugger [StackEntry]
getRemoteThreadIPEStack threadIdRef = do
  !ipe_stack_fv <- compileExprRemote "GHC.Stack.CloneStack.decode Control.Monad.<=< GHC.Stack.CloneStack.cloneThreadStack"
  evalApplicationIOList ipe_stack_fv (castForeignRef threadIdRef)
    >>= \case
      Left (EvalRaisedException e) -> do
        logSDoc Logger.Info (text "Failed to decode the stack with" <+> text (show e) $$ text "This is likely bug #26640 in the decoder, which has been fixed for 9.14.2 and forward. No StackTrace could be produced...")
        return []
      Left e -> do
        logSDoc Logger.Warning (text "Failed to decode the stack with" <+> text (show e) $$ text "No StackTrace will be returned...")
        return []
      Right entries_fvs -> do
        mapM (\entry_fv -> do
            stack_entry <- obtainParsedTerm "StackEntry" 2 True anyTy{-list of stackentry, but we won't look...-} entry_fv stackEntryParser
            case stack_entry of
              Left errs -> do
                logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
                liftIO $ fail "Failed to parse @StackEntry@ from decoding thread stack!"
              Right se ->
                pure se
          ) entries_fvs

--------------------------------------------------------------------------------
-- * TermParsers
--------------------------------------------------------------------------------

threadStatusParser :: TermParser ThreadStatus
threadStatusParser = do
        (matchConstructorTerm "ThreadRunning"  $> ThreadRunning)
    <|> (matchConstructorTerm "ThreadFinished" $> ThreadFinished)
    <|> (matchConstructorTerm "ThreadDied"     $> ThreadDied)
    <|> (matchConstructorTerm "ThreadBlocked"  *> (ThreadBlocked <$> subtermWith 0 blockedReasonParser))

blockedReasonParser :: TermParser BlockReason
blockedReasonParser = do
        (matchConstructorTerm "BlockedOnMVar"        $> BlockedOnMVar)
    <|> (matchConstructorTerm "BlockedOnBlackHole"   $> BlockedOnBlackHole)
    <|> (matchConstructorTerm "BlockedOnException"   $> BlockedOnException)
    <|> (matchConstructorTerm "BlockedOnSTM"         $> BlockedOnSTM)
    <|> (matchConstructorTerm "BlockedOnForeignCall" $> BlockedOnForeignCall)
    <|> (matchConstructorTerm "BlockedOnOther"       $> BlockedOnOther)

stackEntryParser :: TermParser StackEntry
stackEntryParser = do
    StackEntry <$> subtermWith 0 stringParser <*> subtermWith 1 stringParser <*> subtermWith 2 stringParser <*> pure INVALID_OBJECT{-this is a stub-}
