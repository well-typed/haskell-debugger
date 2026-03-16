{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  , listAllLiveRemoteThreads

  -- * Re-exports
  , Debuggee.ThreadInfo(..)
  ) where

import Data.Maybe
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

import Colog.Core as Logger
import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Term.Parser
import GHC.Debugger.Runtime.Thread.Map

import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin as Remote

import qualified GHC.Debugger.Runtime.Interpreter as Debuggee

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

-- | Call 'listThreads' on the (possibly) remote debuggee process to get the
-- list of threads running on the debuggee. Filter by running threads
-- This may include the debugger threads if using the internal interpreter.
listAllLiveRemoteThreads :: Debugger [(RemoteThreadId, Debuggee.ThreadInfo ForeignRef)]
listAllLiveRemoteThreads = do
  threadInfos <- Debuggee.listThreads
  fmap catMaybes $
    forM threadInfos $ \ti -> do
      rti <- getRemoteThreadId ti.threadInfoRef
      pure $ case ti.threadInfoStatus of
        (ThreadRunning ; ThreadBlocked{}) -> Just (rti, ti)
        (ThreadDied    ; ThreadFinished)  -> Nothing

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

