{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Evaluating string-expressions in the context of the source program.
-- This is what is used to evaluate expressions at the REPL while stopped.
--
-- In contrast, @GHC.Debugger.Runtime.Eval@ is for evaluating any and all
-- expressions in the (external) interpreter for the debugger purposes (rather
-- than directly the user's).
module GHC.Debugger.Run.Eval
  ( doEval, addImport
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
import GHC.Debugger.Stopped
import GHC.Debugger.Stopped.Frames
import GHC (TyThing (AnId))
import GHC.Debugger.Data.ThreadMap
import Data.IORef
import Control.Monad.Reader
import qualified Data.IntMap.Strict as IM
import Colog.Core
import Data.List
import GHC.Types.DefaultEnv
import GHC.Core.InstEnv (emptyInstEnv)
import GHC.Types.SourceError
import GHC.TypeLits (Symbol)
import Data.Kind (Type)
import GHC.Unit

-- | Parses input as an import declaration and applies it to the global interactive context.
addImport :: String -> Debugger EvalResult
addImport s = handleError $ withGlobalIC $ do
  idecl <- lift $ parseImportDecl s
  imports <- getGlobalImports
  setGlobalImports (IIDecl idecl : imports)
  pure $ EvalCompleted "" "" Nothing NoVariables
  where
    handleError m = m `MC.catch` \ (e::SourceError) -> do
      pure $ EvalAbortedWith $ displayException e

newtype ScopeT (s :: Symbol) (m :: Type -> Type) a = ScopeT {unsafeUnScopeT :: Scope s -> m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Scope s)) via ReaderT (Scope s) m
  deriving MonadTrans via ReaderT (Scope s)

getGlobalImports :: ScopeT "global" Debugger [InteractiveImport]
getGlobalImports = ic_imports . hsc_IC <$> lift getSession

setGlobalImports :: [InteractiveImport] -> ScopeT "global" Debugger ()
setGlobalImports imports = lift $ do
  (gic,gen) <- getGlobalIC
  GHC.setContext imports
  -- we write only here so we know setContext with the extra argument works.
  new_gic <- hsc_IC <$> getSession
  setGlobalIC (new_gic,gen+1)

withGlobalIC :: ScopeT "global" Debugger a -> Debugger a
withGlobalIC m = withTempInteractiveContext Global $ do
  (gic,_) <- lift getGlobalIC
  lift $ modifySession (\ s -> s {hsc_IC = gic})
  m

-- | Evaluate expression including the interactive context at the breakpoint
-- the given thread is currently stopped at. If the thread is not stopped, the
-- expression is evaluated in the global context.
doEval :: (ExecResult -> Debugger EvalResult)
       -- ^ How to handle a result from evaluation.
       -- Typically this is 'GHC.Debugger.Run.Handler.handleExecResult'.
       -> Maybe (RemoteThreadId, Int)
       -> String
       -> Debugger EvalResult
doEval handleExecResult scope expr = withThreadBreakEnv scope $ do
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

getGlobalIC :: Debugger (InteractiveContext, Int)
getGlobalIC = do
  gicRef <- asks globalIC
  liftIO $ readIORef gicRef

setGlobalIC :: (InteractiveContext, Int) -> Debugger ()
setGlobalIC x = do
  gicRef <- asks globalIC
  liftIO $ writeIORef gicRef x

-- | Pre-condition: inner action does not alter imports.
withThreadBreakEnv :: Maybe (RemoteThreadId, Int) -> Debugger a -> Debugger a
withThreadBreakEnv Nothing m = withGlobalIC $ lift $ do
  x <- m
  ic <- hsc_IC <$> getSession
  (_,gen) <- getGlobalIC
  setGlobalIC (ic,gen)
  pure x
withThreadBreakEnv q@(Just (tid, frame_index)) m = do
  let debugLog (d :: SDoc) = logSDoc Debug (text $ intercalate " " $ lines $ showPprUnsafe $ text "withThreadBreakEnv:" <+> d)
  debugLog (text (show q))
  readStoppedThread tid >>= \case
    Nothing -> do
      throwM $ NonFatalException { userMessage = "Evaluation only supported in stopped threads.", debugMessage = "withThreadBreakEnv: " ++ show q}
    Just st -> do
      debugLog (text "found st")
      withTempInteractiveContext (Frame tid frame_index) $ do
        storeIC <- setIC tid frame_index st

        x <- lift m

        -- persists any additions by `m`, e.g. `it`.
        storeIC
        pure x

refreshImports :: Maybe ImportsEpoch -> Maybe Module -> ScopeT "frame" Debugger (ScopeT "frame" Debugger ())
refreshImports gen m = do
 (gic,gen') <- lift getGlobalIC
 lclImports <- ic_imports . hsc_IC <$> lift getSession
 let extra = case m of
       Nothing -> []
       Just modl -> [IIModule modl]
 let hasImport = case m of
       Nothing -> True
       Just modl -> case lclImports of
         (IIModule mod':_) -> modl == mod'
         _ -> False
 unless (gen == Just gen' && hasImport) $ do
   lift $ GHC.setContext (extra ++ ic_imports gic)
 let w = writeIC (gen',m)
 w
 pure w

data Scope :: Symbol -> Type where
  Global :: Scope "global"
  Frame :: RemoteThreadId -> Int -> Scope "frame"

withTempInteractiveContext :: GhcMonad m => Scope s -> ScopeT s m b -> m b
withTempInteractiveContext scope m = do
  let acquire = do
        ic <- hsc_IC <$> getSession
        dflags <- getInteractiveDebuggerDynFlags
        pure (ic,dflags)
      release (ic,dflags) = do
          modifySession (\ s -> s {hsc_IC = ic})
          setInteractiveDebuggerDynFlags dflags
  MC.bracket acquire release $ const (unsafeUnScopeT m scope)

writeIC :: (GhcMonad m, MonadReader DebuggerState m) => (ImportsEpoch,Maybe Module) -> ScopeT "frame" m ()
writeIC new_gen = do
  s <- ask
  let Frame tid frame_index = s
  ic <- hsc_IC <$> lift getSession
  trm_ref <- lift $ asks threadResumeMap
  liftIO $
    modifyIORef' trm_ref $ \tm -> do
      let rtid = (remoteThreadIntRef tid)
      case lookupThreadMap rtid tm of
        Just StoppedThread{..} ->
          insertThreadMap rtid
          (StoppedThread {frameICs = IM.insert frame_index (ic,new_gen) frameICs,..})
          tm
        Nothing -> tm

setIC :: RemoteThreadId -> IM.Key -> StoppedThread -> ScopeT "frame" Debugger (ScopeT "frame" Debugger ())
setIC tid frame_index StoppedThread{..}
  | Just (ic,(gen,extra)) <- IM.lookup frame_index frameICs
   = do
    lift $ modifySession (\ s -> s {hsc_IC = ic})
    refreshImports (Just gen) extra
  | otherwise = do
  let warn (d :: SDoc) = logSDoc Debug (text "Evaluating in " <+> text (show (tid,frame_index)) <+> text ":" <+> d)
  hug  <- hsc_HUG <$> lift getSession
  mibi <- lift $ do
    if frame_index == 0 then do
      modifySession (\ s -> s {hsc_IC = breakIC})
      -- resume is only relevant for frame_index == 0.
      mibi <- case resumeBreakpointId resume of
        Just ibi -> pure (Just ibi)
        Nothing -> ((.breakId) =<<) <$> getStackFrame tid frame_index
      pure mibi
    else do
      getStackFrame tid frame_index >>= \case
        Nothing -> do
          warn $ text "Could not retrieve stack frame, no bindings added to the scope."
          pure Nothing
        Just fr -> do
          addBindingsIfNew . map AnId =<< getStackFrameBindings fr
          pure fr.breakId
  case mibi of
    Nothing -> do
      lift $ warn $ text "Could not retrieve breakpoint id, no break module added to the scope."
      refreshImports Nothing Nothing
    Just ibi -> do
      breakModule <- getBreakSourceMod ibi <$> readIModBreaks hug ibi & liftIO
      lift $ addModuleDynFlags breakModule
      refreshImports Nothing (Just breakModule)
   where
    addBindingsIfNew tythings = do
      s <- getSession
      let ic = hsc_IC s
      setSession $ s{hsc_IC = extendInteractiveContextWithTyThings ic tythings }
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
extendInteractiveContextWithTyThings ictxt new_tythings
  | null new_tythings = ictxt
  | otherwise
  = extendInteractiveContext ictxt new_tythings emptyInstEnv [] emptyDefaultEnv (ic_fix_env ictxt)
