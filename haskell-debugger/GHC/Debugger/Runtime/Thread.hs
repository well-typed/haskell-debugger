{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- TODO
-- - [] Consider caching once and forall the expressions we dynamically compile and load in this module.
module GHC.Debugger.Runtime.Thread
  ( getRemoteThreadIdFromRemoteContext
  , getRemoteThreadId
  , getRemoteThreadsLabels
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

import GHC.Builtin.Types
import GHC.Runtime.Heap.Inspect
import GHC.Utils.Outputable

import GHCi.Message
import GHCi.RemoteTypes

import GHC.Debugger.Utils
import GHC.Debugger.Logger as Logger
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Term.Parser
import GHC.Debugger.Runtime.Thread.Map

import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin as Remote

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
  thread_id_fv <- expectRight =<< Remote.eval
    (Remote.fromThreadId (Remote.ref threadIdRef))

  parsed_int <-
    obtainParsedTerm "ThreadId's Int value" 2 True wordTy{-really, Word64, but we won't look at the type-} (castForeignRef thread_id_fv) intParser

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
  status_fv  <- expectRight =<< Remote.evalIO
    (Remote.threadStatus (Remote.ref threadIdRef))
  status_parsed <-
    obtainParsedTerm "ThreadStatus" 2 True anyTy{-..no..-} (castForeignRef status_fv) threadStatusParser

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
  threads_fvs <- expectRight =<< Remote.evalIOList Remote.listThreads
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

  forM threadIdRefs $ \threadIdRef -> do

    r <- Remote.evalIOList $ Remote.do
      mb_str <- Remote.threadLabel (Remote.ref threadIdRef)
      Remote.return (Remote.maybeToList mb_str)

    expectRight r >>= \case
      []          -> pure Nothing
      [io_lbl_fv] -> Just <$> (expectRight =<< Remote.evalString (Remote.ref io_lbl_fv))
      _ -> liftIO $ fail "Unexpected result from evaluating \"threadLabel\""

--------------------------------------------------------------------------------
-- * TermParsers
--------------------------------------------------------------------------------

-- ** Threads ------------------------------------------------------------------

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

