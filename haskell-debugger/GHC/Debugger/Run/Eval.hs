{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

-- | Evaluating string-expressions in the context of the source program.
-- This is what is used to evaluate expressions at the REPL while stopped.
--
-- In contrast, @GHC.Debugger.Runtime.Eval@ is for evaluating any and all
-- expressions in the (external) interpreter for the debugger purposes (rather
-- than directly the user's).
module GHC.Debugger.Run.Eval
  ( doEval, withThreadBreakEnv, withTempInteractiveContext
  ) where
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Monad
import GHC.Debugger.Session
import GHC.Debugger.Runtime.Thread.Resume
import GHC.Debugger.Run.Resume
import Control.Exception
import GHC.Runtime.Eval hiding (resumeExec)
import GHC.Hs
import GHC.Driver.Session
import GHC.Runtime.Context
import qualified GHC
import GHC.Unit.Module.ModSummary
import GHC.ByteCode.Breakpoints
import GHC.Driver.Monad
import Control.Monad
import GHC.Driver.Env
import GHC.Driver.Main
import GHC.Types.SrcLoc
import Control.Monad.Catch as MC
import Data.Function
import GHC.Debugger.Utils
import Data.Maybe
import GHC.Debugger.Stopped
import GHC.Debugger.Stopped.Frames
import qualified Data.Map as Map
import GHC (lookupName, TyThing (AnId), NamedThing (getName))
import GHC.Debugger.Data.ThreadMap
import Data.IORef
import Control.Monad.Reader
import qualified Data.IntMap.Strict as IM
import Colog.Core
import Data.List
import GHC.Types.DefaultEnv
import GHC.Core.InstEnv (emptyInstEnv)
import GHC.Builtin.Names

-- | Evaluate expression including the interactive context at the breakpoint
-- the given thread is currently stopped at. If the thread is not stopped, the
-- expression is evaluated in the global context.
doEval :: (ExecResult -> Debugger EvalResult)
       -- ^ How to handle a result from evaluation.
       -- Typically this is 'GHC.Debugger.Run.Handler.handleExecResult'.
       -> Maybe (RemoteThreadId, Int)
       -> String
       -> Debugger EvalResult
doEval handleExecResult scope expr = withThreadBreakEnv scope True $ do
  excr <- (Right <$> exec expr execOptions) `MC.catch` \(e::SomeException) -> pure (Left (displayException e))
  case excr of
    Left err -> pure $ EvalAbortedWith err
    Right (k, br@ExecBreak{}) -> fmap (addSourceKind k) $ execBreakResume br >>= continueToCompletion >>= handleExecResult
    Right (k, r@ExecComplete{}) -> fmap (addSourceKind k) $ handleExecResult r
  where
    exec :: String -> ExecOptions -> Debugger (SourceKind, ExecResult)
    exec input exec_opts@ExecOptions{..} = do
      hsc_env <- getSession

      mb_stmt <-
        liftIO $
        runInteractiveHsc hsc_env $
        hscParseStmtWithLocation execSourceFile execLineNumber input

      case mb_stmt of
        -- empty statement / comment
        Nothing -> return (IsStmt, ExecComplete (Right []) 0)
        Just stmt -> (,) <$> stmtKind stmt <*> execStmt' stmt input exec_opts

    stmtKind (stmt :: GhciLStmt GhcPs) = do
      pure $ case unLoc stmt of
        BodyStmt{} -> IsExpr
        _ -> IsStmt

    addSourceKind :: SourceKind -> EvalResult -> EvalResult
    addSourceKind k EvalCompleted{..} = EvalCompleted{resultSourceKind = Just k, ..}
    addSourceKind _ r = r

-- | Resume execution with single step mode 'RunToCompletion', skipping all breakpoints we hit, until we reach 'ExecComplete'.
--
-- We use this in 'doEval' because we want to ignore breakpoints in expressions given at the prompt.
continueToCompletion :: Resume -> Debugger ExecResult
continueToCompletion r = do
  execr <- resumeExec RunToCompletion Nothing r
  case execr of
    br@ExecBreak{} -> execBreakResume br >>= continueToCompletion
    ExecComplete{} -> return execr

withThreadBreakEnv :: Maybe (RemoteThreadId, Int) -> Bool -> Debugger a -> Debugger a
withThreadBreakEnv q@Nothing persist                    m = withTempInteractiveContext $ do
  gicRef <- asks globalIC
  ic0 <- liftIO $ readIORef gicRef
  modifySession $ \ s -> s {hsc_IC = ic0}
  -- global env? TODO: make sure local evals and DebugBreak from DebugExecution don't pollute this.
  setContext =<< liftIO . readIORef =<< asks globalImports
  let debugLog (d :: SDoc) = logSDoc Debug (text $ intercalate " " $ lines $ showPprUnsafe $ text "withThreadBreakEnv:" <+> d)
  debugLog (text (show q) <+> text "_")
  x <- m
  ic <- hsc_IC <$> getSession
  liftIO $ writeIORef gicRef ic
  pure x
withThreadBreakEnv q@(Just (tid, frame_index)) persist m = do
  let debugLog (d :: SDoc) = logSDoc Debug (text $ intercalate " " $ lines $ showPprUnsafe $ text "withThreadBreakEnv:" <+> d)
  debugLog (text (show q) <+> ppr persist)
  readStoppedThread tid >>= \case
    Nothing -> do
      debugLog (text "no res")
      -- TODO: thread not stopped error.
      m
    Just st -> do
      debugLog (text "found st")
      withTempInteractiveContext $ do
        setIC tid frame_index st

        when persist $ do
          writeIC tid frame_index st

        x <- m

        -- persists any additions by `m`, e.g. `it`.
        when persist $ do
          writeIC tid frame_index st
        pure x

withTempInteractiveContext :: GhcMonad m => m b -> m b
withTempInteractiveContext m = do
  let acquire = do
        ic <- hsc_IC <$> getSession
        dflags <- getInteractiveDebuggerDynFlags
        pure (ic,dflags)
      release (ic,dflags) = do
          modifySession (\ s -> s {hsc_IC = ic})
          setInteractiveDebuggerDynFlags dflags
  MC.bracket acquire release $ const m

writeIC :: (GhcMonad m, MonadReader DebuggerState m) => RemoteThreadId -> IM.Key -> StoppedThread -> m ()
writeIC tid frame_index StoppedThread{..} = do
  ic <- hsc_IC <$> getSession
  trm_ref <- asks threadResumeMap
  liftIO $
    modifyIORef' trm_ref $
      insertThreadMap (remoteThreadIntRef tid)
        (StoppedThread {frameICs = IM.insert frame_index ic frameICs,..})

setIC :: RemoteThreadId -> IM.Key -> StoppedThread -> Debugger ()
setIC tid frame_index st@StoppedThread{..} = do
  case IM.lookup frame_index frameICs of
     Just ic -> modifySession (\ s -> s {hsc_IC = ic})
     Nothing | frame_index == 0 -> modifySession (\ s -> s {hsc_IC = breakIC})
             | otherwise -> pure ()
  let debugLog (d :: SDoc) = logSDoc Debug (text $ intercalate " " $ lines $ showPprUnsafe $ text "withThreadBreakEnv:" <+> d)
  hug  <- hsc_HUG <$> getSession
  (mibi,bindings) <-
    if frame_index == 0 then do
      -- resume is only relevant for frame_index == 0.
      mibi <- case resumeBreakpointId resume of
        Just ibi -> pure (Just ibi)
        Nothing -> ((.breakId) =<<) <$> getStackFrame tid frame_index
      let breakTyThings = ic_tythings breakIC
      debugLog (text "breakTyThings = " <+> (ppr breakTyThings) <+> (ppr (map (getUnique . getName) breakTyThings)))
      pure (mibi,breakTyThings)
    else do
      getStackFrame tid frame_index >>= \case
        Nothing -> do
          debugLog (text "No bindings because no frame, frame_index:" <+> ppr frame_index)
          pure (Nothing,[]) -- TODO: warn.
        Just fr -> (fr.breakId,) . map AnId <$> getStackFrameBindings fr

  debugLog (text "bindings" <+> ppr bindings)
  addBindingsIfNew bindings
  case mibi of
    Nothing -> pure ()
    Just ibi -> do
      breakModule <- getBreakSourceMod ibi <$> readIModBreaks hug ibi & liftIO
      addModuleDynFlags breakModule
      addImports (IIModule breakModule:)
   where
    addBindingsIfNew tythings = do
      s <- getSession
      let ic = hsc_IC s
      setSession $ s{hsc_IC = extendInteractiveContextWithTyThings ic tythings }
    addImports f = do
      cxt <- liftIO . readIORef =<< asks globalImports
      -- TODO: IC interface should allow adding to the context, rather than a full setContext.
      setContext (f cxt)
    addModuleDynFlags breakModule = do
      ic_dyn_flags <- getInteractiveDebuggerDynFlags
      break_dyn_flags <- ms_hspp_opts <$> GHC.getModSummary breakModule
      setInteractiveDebuggerDynFlags $ adjustFlags ic_dyn_flags break_dyn_flags

    -- Possibly we might want to include more from the module's DynFlags.
    -- However some are likely to mess with the REPL, e.g. Opt_WarnTypeDefaults,
    -- Opt_HideAllPackages, Opt_NoIt. See discussion at
    -- https://github.com/well-typed/haskell-debugger/pull/230#discussion_r2986758826
    adjustFlags :: DynFlags -> DynFlags -> DynFlags
    adjustFlags ic modl = ic
      { extensions = extensions modl
      , extensionFlags = extensionFlags modl
      , language = language modl
      }

extendInteractiveContextWithTyThings :: InteractiveContext -> [TyThing] -> InteractiveContext
-- Just a specialised version
extendInteractiveContextWithTyThings ictxt new_tythings0
  | Map.null new_tythings = ictxt
  | otherwise
  = extendInteractiveContext ictxt (Map.elems new_tythings) emptyInstEnv [] emptyDefaultEnv (ic_fix_env ictxt)
  where
    new_tythings = namesSet new_tythings0 Map.\\ namesSet (ic_tythings ictxt)
    namesSet xs = Map.fromList $ map (\ x -> (GHC.getName x,x)) xs
