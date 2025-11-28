{-# LANGUAGE LambdaCase #-}
-- |
-- TODO
-- - [] Consider caching once and forall the expressions we dynamically compile and load in this module.
module GHC.Debugger.Runtime.Thread
  ( getRemoteThreadIdFromRemoteContext
  , getRemoteThreadId
  , getRemoteThreadsLabels
  , getRemoteThreadStackCopy
  , listAllRemoteThreads
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef

import GHC
import GHC.Builtin.Types
import GHC.Runtime.Heap.Inspect

import GHC.Driver.Config
import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp

import GHCi.Message
import GHCi.RemoteTypes

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Thread.Map

-- | Get a 'RemoteThreadId' from a remote 'ResumeContext' gotten from an 'ExecBreak'
getRemoteThreadIdFromRemoteContext :: ForeignRef (ResumeContext [HValueRef]) -> Debugger RemoteThreadId
getRemoteThreadIdFromRemoteContext fctxt = do
  hsc_env <- getSession

  -- Get the ResumeContext term and fetch the resumeContextThreadId field
  liftIO (cvObtainTerm hsc_env 2 True anyTy (castForeignRef fctxt)) >>= \case
    Term{subTerms=[_mvar1, _mvar2, threadIdTerm@Term{}]} -> do

      getRemoteThreadId (castForeignRef (val threadIdTerm))

    _ -> error "Expecting ResumeContext term!!"


getRemoteThreadId :: ForeignRef ThreadId -> Debugger RemoteThreadId
getRemoteThreadId threadIdRef = do
      hsc_env <- getSession

      -- evalStmt takes an IO [a] and evals it into a [ForeignHValue]
      from_thread_id_fv <- compileExprRemote "(pure @IO . (:[]) . GHC.Conc.Sync.fromThreadId)"

      let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
          interp    = hscInterp hsc_env

          handleSingleStatus [func_fv] = return func_fv
          handleSingleStatus l         = fail $
            "Unexpected result when loading \"fromThreadId\" function (" ++ show (length l) ++ ")"

      r_term <- liftIO $
        cvObtainTerm hsc_env 2 True wordTy{-really, Word64, but we won't look at the type-}
        =<< handleSingleStatus =<< handleStatus hsc_env =<<
        evalStmt interp eval_opts
          (EvalApp (EvalThis from_thread_id_fv) (EvalThis (castForeignRef threadIdRef)))

      case r_term of
        Term{subTerms=[Prim{valRaw=[w64_tid]}]} -> do

          let i_tid = fromIntegral w64_tid :: Int

          tmap_ref <- asks threadMap
          -- unconditionally write to the map the foreign ref (it should always
          -- refer to the same ThreadId as a possible existing entry)
          liftIO $ modifyIORef' tmap_ref $
            insertThreadMap i_tid threadIdRef

          return (RemoteThreadId i_tid)
        _ -> liftIO $ fail $ "Unexpected term result from \"fromThreadId\""

-- | Call 'listThreads' on the (possibly) remote debuggee process to get the list of threads running on the debuggee.
-- This may include the debugger threads if using the internal interpreter.
listAllRemoteThreads :: Debugger [(RemoteThreadId, ForeignRef ThreadId)]
listAllRemoteThreads = do
  hsc_env <- getSession

  -- evalStmt will take this IO [ThreadId] and eval it to [ForeignHValue]
  list_threads_fv <- compileExprRemote "GHC.Conc.Sync.listThreads"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp    = hscInterp hsc_env

  liftIO (evalStmt interp eval_opts (EvalThis list_threads_fv))
    >>= liftIO . handleStatus hsc_env >>= \case
      threads_fvs -> do

        forM threads_fvs $ \thread_fv -> do
          -- TODO: awful to compile the expression to get the remote id every single time...
          tid <- getRemoteThreadId (castForeignRef thread_fv)
          pure (tid, castForeignRef thread_fv)

-- | Get the label of a Thread in a remote process. Returns one element per
-- given Thread with @Just string@ if a label was found.
getRemoteThreadsLabels :: [ForeignRef ThreadId] -> Debugger [Maybe String]
getRemoteThreadsLabels threadIdRefs = do
  hsc_env <- getSession

  -- evalStmt takes an IO [a] and evals it into a [ForeignHValue]. Represent the Maybe as an empty list
  thread_label_fv <- compileExprRemote "(fmap (\\ms -> case ms of Nothing -> []; Just s -> [pure s]) \
                                           . GHC.Conc.Sync.threadLabel \
                                            ) :: ThreadId -> IO [IO String]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp    = hscInterp hsc_env

  forM threadIdRefs $ \threadIdRef -> liftIO $
    evalStmt interp eval_opts
      (EvalApp (EvalThis thread_label_fv) (EvalThis (castForeignRef threadIdRef)))
        >>= handleStatus hsc_env >>= \case
          []          -> pure Nothing
          [io_lbl_fv] -> Just <$> evalString interp io_lbl_fv
          _ -> fail "Unexpected result from evaluating \"threadLabel\""

-- | Clone the stack of the given remote thread and get the Term
getRemoteThreadStackCopy :: ForeignRef ThreadId -> Debugger Term
getRemoteThreadStackCopy threadIdRef = do
  hsc_env <- getSession

  -- evalStmt takes an IO [a] and evals it into a [ForeignHValue]. Represent the Maybe as an empty list
  thread_stack_fv <- compileExprRemote "fmap (:[]) . GHC.Exts.Stack.decodeStack Control.Monad.<=< GHC.Stack.CloneStack.cloneThreadStack"

  -- TODO: Currently, GHC.Stack.CloneStack.decode, which uses the IPE
  -- information to report source locations of the callstacks, does not work
  -- for a stack with interpreter return frames. We would probably also like to
  -- use that.

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp    = hscInterp hsc_env

  liftIO $ evalStmt interp eval_opts
    (EvalApp (EvalThis thread_stack_fv) (EvalThis (castForeignRef threadIdRef)))
      >>= handleStatus hsc_env >>= \case
        [stack_frames_fv] ->
          cvObtainTerm hsc_env 2 True anyTy{-todo:stackframety-} stack_frames_fv
        _ -> fail "Unexpected result from evaluating \"threadLabel\""

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

handleStatus :: HscEnv -> EvalStatus_ [ForeignHValue] [HValueRef] -> IO [ForeignHValue]
handleStatus hsc_env (EvalBreak _ _ resume_ctxt _) = do
  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp    = hscInterp hsc_env
  resume_ctxt_fhv <- mkFinalizedHValue interp resume_ctxt
  handleStatus hsc_env =<< Interp.resumeStmt interp eval_opts resume_ctxt_fhv
handleStatus _ (EvalComplete _ (GHCi.Message.EvalException e)) =
  throwIO (fromSerializableException e)
handleStatus _ (EvalComplete _ (EvalSuccess ls)) =
  return ls
