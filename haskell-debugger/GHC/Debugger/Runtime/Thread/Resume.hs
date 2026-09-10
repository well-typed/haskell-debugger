{-# LANGUAGE LambdaCase, ViewPatterns, CPP #-}
module GHC.Debugger.Runtime.Thread.Resume where

import GHC.Debugger.Interface.Messages
import GHC.Runtime.Eval
import GHC.Debugger.Monad
import Data.IORef
import Control.Monad.Reader
import Data.Function
import GHC.Debugger.Runtime.Thread.Map
import GHC.Debugger.Runtime.Thread

-- | Pop the resume for this thread off
popResume :: RemoteThreadId -> Debugger Resume
popResume rti = do
  trm_ref <- asks threadResumeMap
  readResume rti >>= \case
    Nothing -> error "Trying to pop the resume of a thread which isn't running?"
    Just tr -> do
      modifyIORef' trm_ref (deleteThreadMap (remoteThreadIntRef rti)) & liftIO
      pure tr

-- | Push the resume for its thread to the mapping keeps track of which threads
-- are paused
pushResume :: Resume -> Debugger ()
pushResume res = do
  trm_ref <- asks threadResumeMap
  rti     <- getResumeThreadId res
  liftIO $
    modifyIORef' trm_ref $
      insertThreadMap (remoteThreadIntRef rti) res

-- | Get the 'Resume' a given stopped thread is currently at.
-- Returns Nothing if we're no longer stopped at this thread
--
-- See Note [Don't crash if not stopped] for why we may try to get the resume
-- of a thread we're no longer stopped at.
readResume :: RemoteThreadId -> Debugger (Maybe Resume)
readResume (remoteThreadIntRef -> rti) = do
  trm_ref <- asks threadResumeMap
  trm     <- readIORef trm_ref & liftIO
  case lookupThreadMap rti trm of
    Nothing -> pure Nothing
    Just tr -> pure (Just tr)

getResumeThreadId :: Resume -> Debugger RemoteThreadId
#if MIN_VERSION_ghc(10,1,0)
getResumeThreadId = getRemoteThreadId . resumeContext
#else
getResumeThreadId = getRemoteThreadIdFromRemoteContext . resumeContext
#endif

-- | Get the 'Resume' associated to the given breakpoint result 'ExecBreak'.
--
-- On GHCs with better multi-threaded debugger support (>= 10.1), the 'Resume'
-- associated to a breakpoint is returned directly in 'ExecBreak'.
--
-- On older GHCs, the breakpoint resume is pushed to `ic_resume` context as
-- soon as it is hit.
--
-- WARNING: if we can hit and detect more breakpoints simultaneously, looking at
-- the head of ic_resume will be wrong. And there's no information we can use
-- to uniquely identify the right Resume in `ic_resume` for this breakpoint.
-- FIXME: Therefore, we shouldn't look for breakpoints simultaneously in 9.14.2?
-- Either that, or copy all the code over which returns the ExecBreak and
-- modify it (ie copy `handleRunStatus`).
execBreakResume :: ExecResult -> Debugger Resume
#if MIN_VERSION_ghc(10,1,0)
execBreakResume ExecBreak{breakResume} = pure breakResume
#else
execBreakResume ExecBreak{} =
  getResumeContext >>= \case
    r:_ -> pure r
    []  -> error "execBreakResume: stopped at a break but the resume context is empty?!"
#endif
execBreakResume ExecComplete{} =
  error "execBreakResume: expected ExecBreak but got ExecComplete"
