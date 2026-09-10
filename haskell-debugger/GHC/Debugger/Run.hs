{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wredundant-record-wildcards #-} -- bc CPP

-- | Start debugging a program, evaluate statements, and resume stopped threads.
module GHC.Debugger.Run
  (
  -- * Start debugging a program
    debugExecution

  -- * Resume stopped program
  , doResume

  -- * Interactive evaluation
  , doEvalCommand
  ) where

import GHC.Utils.Outputable
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Function

import GHC qualified
import GHC (
  ExecResult (..),
  execStmt',
  ForeignHValue,
  GhciLStmt,
  GhcPs,
  InteractiveImport (..),
  mkHsString,
  ModSummary (..),
  Name,
  nlHsLit,
  nlList,
  parseImportDecl,
  SingleStep (..),
  SrcSpan (..),
  StmtLR (..),
  unLoc,
  )
import GHC.Plugins (SourceError)
#if MIN_VERSION_ghc(10,1,0)
import GHC.Builtin.Modules (gHC_INTERNAL_GHCI_HELPERS)
#else
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
#endif
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Main (hscParseStmtWithLocation)
import GHC.Driver.Monad as GHC
import GHC.Driver.Env as GHC
import qualified GHC.Driver.Config.Parser as GHC
import GHC.Runtime.Debugger.Breakpoints as GHC
import GHC.Types.Name.Occurrence (mkVarOccFS)
import GHC.Types.Name.Reader as RdrName (mkOrig)
import qualified GHCi.Message as GHCi

import GHC.Debugger.Stopped.Variables
import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Interface.Messages
import Colog.Core as Logger
import qualified GHC.Debugger.Breakpoint.Map as BM
import GHC.Debugger.Runtime.Thread
import GHC.Debugger.Runtime.Thread.Map
import GHC.Debugger.Runtime.Thread.Resume
import GHC.Debugger.Session (setInteractiveDebuggerDynFlags, getInteractiveDebuggerDynFlags, resumeExec)
import Data.List (find)
import GHC.Unit.Module.Graph as GHC
import GHC.Types.Var
import GHC.Runtime.Eval.Types
import GHC.Runtime.Eval (readIModBreaks)
import GHC.ByteCode.Breakpoints

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

-- | Run a program with debugging enabled
debugExecution :: AbsFilePath -> EntryPoint -> [String] {-^ Args -} -> Debugger EvalResult
debugExecution entryFile entry args = do
  -- consider always using :trace like ghci-dap to always have a stacktrace?
  -- better solution could involve profiling stack traces or from IPE info?
  modSummaryOfEntryFile <- findUnitIdOfEntryFile entryFile
  let modOfEntryFile = GHC.moduleNodeInfoModule modSummaryOfEntryFile
      unitIdOfEntryFile = GHC.moduleNodeInfoUnitId modSummaryOfEntryFile

  let
    evalModule = mkModule (RealUnit (Definite unitIdOfEntryFile))
                                         (moduleName modOfEntryFile)

  logSDoc Logger.Debug $ "Eval inputs: " <+> text (show (entryFile,entry,args))
  logSDoc Logger.Debug $ "Eval Module Context:" <+> withPprStyle (PprDump reallyAlwaysQualify) (ppr evalModule) <+> ppr (moduleNodeInfoLocation modSummaryOfEntryFile)

  old_context <- GHC.getContext
  GHC.setContext [IIModule evalModule]

  (entryExp, exOpts) <- case entry of
    MainEntry nm -> do
      let prog = fromMaybe "main" nm
      -- the wrapper is equivalent to GHCi's `:main arg1 arg2 arg3`
      wrapper <- mkEvalWrapper prog args -- bit weird that the prog name is the expression but fine
      let execWrap' fhv = GHCi.EvalApp (GHCi.EvalThis wrapper) (GHCi.EvalThis fhv)
          opts = GHC.execOptions {execWrap = execWrap'}
      return (prog, opts)

    FunctionEntry fn ->
      -- TODO: if "args" is unescaped (e.g. "some", "thing"), then "some" and
      -- "thing" will be interpreted as variables. To pass strings it needs to
      -- be "\"some\"" "\"things\"".
      return (fn ++ " " ++ unwords args, GHC.execOptions)

  logSDoc Logger.Debug "Compiled wrapper."

  exec_res <- GHC.execStmt entryExp exOpts
#if MIN_VERSION_ghc(10,1,0)
    { execIsolateMode = GHC.MultiThreadedBreaks } -- yeah!
#endif

  logSDoc Logger.Debug $ "Executed entryExp: " <+> text entryExp

  GHC.setContext old_context

  res <- handleExecResult exec_res
  logSDoc Logger.Debug $ "Computed EvalResult."
  pure res
  where
    -- It's not ideal to duplicate these two functions from ghci, but its unclear where they would better live. Perhaps next to compileParsedExprRemote? The issue is run
    mkEvalWrapper :: GhcMonad m => String -> [String] -> m ForeignHValue
    mkEvalWrapper progname' args' =
      runInternal $ GHC.compileParsedExprRemote
      $ evalWrapper' `GHC.mkHsApp` nlHsString progname'
                     `GHC.mkHsApp` nlList (map nlHsString args')
      where
        nlHsString = nlHsLit . mkHsString
        evalWrapper' =
          GHC.nlHsVar $ RdrName.mkOrig gHC_INTERNAL_GHCI_HELPERS (mkVarOccFS (fsLit "evalWrapper"))

    -- run internal here serves to overwrite certain flags while executing the
    -- internal "evalWrapper" computation which is not relevant to the user.
    runInternal :: GhcMonad m => m a -> m a
    runInternal =
        withTempSession mkTempSession
      where
        mkTempSession = hscUpdateFlags (\dflags -> dflags
          { -- Disable dumping of any data during evaluation of GHCi's internal expressions. (#17500)
            dumpFlags = mempty
          }
              -- We depend on -fimplicit-import-qualified to compile expr
              -- with fully qualified names without imports (gHC_INTERNAL_GHCI_HELPERS above).
              `gopt_set` Opt_ImplicitImportQualified
          )

    findUnitIdOfEntryFile :: GhcMonad m => AbsFilePath -> m GHC.ModuleNodeInfo
    findUnitIdOfEntryFile afp = do
      modSums <- getAllLoadedModulesWithPaths
      case find ((== unAbs afp) . unAbs . fst) modSums of
        Nothing -> do
          let norms = map fst modSums
          error $ "findUnitIdOfEntryFile: no unit id found for: " ++ unAbs afp ++ "\nCandidates were:\n" ++ unlines (map show norms)
        Just (_,summary) -> pure summary

doResume :: RemoteThreadId -> ResumeStep -> ResumeTheWorld -> Debugger EvalResult
doResume tid step stop_world = do
  resume <- popResume tid
  ss     <- resolveStep resume step
  -- TODO: stop_world toggles global hit breakpoints mode (todo: look at mode
  -- to determine how to stop) and here run all the threads accordingly.
  -- TODO: for the first iteration, simply call pause on all threads (except
  -- for those with specific debugger labels); but pause will only set step_in,
  -- and won't be able to pause compiled threads, but that is follow up work.
  resumeExec ss Nothing resume
    >>= handleExecResult

-- | Construct the 'SingleStep' resume option for a paused thread from its
-- 'Resume' info and the type of step to do.
resolveStep :: Resume -> ResumeStep -> Debugger SingleStep
resolveStep r = \case
  ResumeNoStep     -> pure RunToCompletion
  ResumeSingleStep -> pure SingleStep
  ResumeStepLocal
    | Nothing <- rbid -- exception
    -> pure SingleStep
    | Just ibi <- rbid
    -> do
       spn <- enclosing_decl ibi
       pure LocalStep { breakAt = RealSrcSpan spn mempty }
  ResumeStepOut
    | Nothing <- rbid
    -> pure StepOut { initiatedFrom = Nothing }
    | Just ibi <- rbid
    -> do
       spn <- enclosing_decl ibi
       pure StepOut { initiatedFrom = Just (RealSrcSpan spn mempty) }
  where
    rbid = resumeBreakpointId r
    loc  = resumeSpan r
    -- To do a local step, we get the SrcSpan of the current suspension state
    -- and get its 'enclosingTickSpan' to use as a filter for breakpoints in
    -- the call to 'resumeExec'. Execution will only stop at breakpoints whose
    -- span matches this enclosing span.
    enclosing_decl ibi = do
      hug   <- hsc_HUG <$> getSession
      brks  <- readIModBreaks hug ibi & liftIO
      let md = getBreakSourceMod ibi brks
      ticks <- fromMaybe (error "doLocalStep:getTicks") <$> makeModuleLineMap md
      pure $ enclosingTickSpan ticks loc

-- | Generalized `doEval` that also handles `imports`
doEvalCommand :: String -> Debugger EvalResult
doEvalCommand expr = do
  dflags <- getInteractiveDebuggerDynFlags
  let pflags = GHC.initParserOpts dflags
  if GHC.isStmt pflags expr
    then doEval expr
    else addImport expr

-- | Parses input as an import declaration and applies it to the interactive context.
addImport :: String -> Debugger EvalResult
addImport s = handleError $ do
  cxt <- GHC.getContext
  idecl <- parseImportDecl s
  GHC.setContext $ IIDecl idecl : cxt
  pure $ EvalCompleted "" "" Nothing NoVariables
  where
    handleError m = m `catch` \ (e::SourceError) -> do
      pure $ EvalAbortedWith $ displayException e

-- | Evaluate expression. Includes context of breakpoint if stopped at one (the current interactive context).
doEval :: String -> Debugger EvalResult
doEval expr = withCurrentBreakEnv $ do
  excr <- (Right <$> exec expr GHC.execOptions) `catch` \(e::SomeException) -> pure (Left (displayException e))
  case excr of
    Left err -> pure $ EvalAbortedWith err
    Right (k, ExecBreak{breakResume}) -> fmap (addSourceKind k) $ continueToCompletion breakResume >>= handleExecResult
    Right (k, r@ExecComplete{}) -> fmap (addSourceKind k) $ handleExecResult r
  where
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
continueToCompletion :: Resume -> Debugger GHC.ExecResult
continueToCompletion r = do
  execr <- resumeExec GHC.RunToCompletion Nothing r
  case execr of
    GHC.ExecBreak{breakResume} -> continueToCompletion breakResume
    GHC.ExecComplete{} -> return execr

-- | @withCurrentBreakEnv m@ executes @m@ with the imports, language, and language
--  extensions of the current breakpoint source module.
--
--  If we are not stopped at a breakpoint @m@ is executed with no change.
withCurrentBreakEnv :: Debugger a -> Debugger a
withCurrentBreakEnv m = do
  mmodl <- getCurrentBreakModule
  case mmodl of
    Nothing          -> m
    Just breakModule -> do
      ic_dyn_flags <- getInteractiveDebuggerDynFlags
      break_dyn_flags <- ms_hspp_opts <$> GHC.getModSummary breakModule
      old_context <- GHC.getContext
      setInteractiveDebuggerDynFlags $ adjustFlags ic_dyn_flags break_dyn_flags
      GHC.setContext (IIModule breakModule : old_context)
      x <- m
      GHC.setContext old_context
      setInteractiveDebuggerDynFlags ic_dyn_flags
      return x
  where
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

-- | Turn a GHC's 'ExecResult' into an 'EvalResult' response
handleExecResult :: GHC.ExecResult -> Debugger EvalResult
handleExecResult = \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> return (EvalCompleted "" "" Nothing NoVariables) -- Evaluation completed without binding any result.
#if MIN_VERSION_ghc(10,1,0)
        Right (n:_ns) -> inspectId n >>= \case
#else
        Right (n:_ns) -> inspectName n >>= \case
#endif
          Just VarInfo{varValue, varType, varRef} -> do
            return (EvalCompleted varValue varType Nothing varRef)
          Nothing     -> liftIO $ fail "doEval failed"
    ExecBreak {breakNames = _, breakPointId = Nothing, ..} -> do
      -- Stopped at an exception
      -- TODO: force the exception to display string with Backtrace?
#if MIN_VERSION_ghc(10,1,0)
      pushResume breakResume
      rt_id <- getResumeThreadId breakResume
#else
      rt_id <- getRemoteThreadIdFromContext
#endif
      return EvalStopped{ breakId = Nothing
                        , breakThread = rt_id }
    ExecBreak {breakNames = _, breakPointId = Just bid, ..} -> do

      pushResume breakResume

      let performAction BreakpointStop = do

#if MIN_VERSION_ghc(10,1,0)
                rt_id <- getResumeThreadId breakResume
#else
                rt_id <- getRemoteThreadIdFromContext
#endif
                return EvalStopped{ breakId = Just bid
                                  , breakThread = rt_id }
          performAction (BreakpointLogAndResume logExpr) = do
            let evalFailedMsg e = text $ unlines ["Evaluation of log message expression failed with " ++ e
                  , "Expr: " ++ logExpr
                  , "Ignoring..."]
            doEval' logExpr evalFailedMsg breakResume $ \ _ _ -> resume breakResume

      bm <- liftIO . readIORef =<< asks activeBreakpoints
      case BM.lookup bid bm of
        -- When stepping (`GHC.resumeExec SingleStep` or similar), we will typically stop at locations not explicitly enabled by the user (i.e. not registered in `activeBreakpoints`).
        Nothing -> performAction BreakpointStop
        Just BreakpointInfo{bpInfoStatus = status, bpInfoAction = action} -> do
          case status of
           -- todo: BreakpointAfterCountCond is not handled yet.
            BreakpointAfterCountCond{} -> performAction action
            BreakpointWhenCond cond -> do
              let evalFailedMsg e = text $ "Evaluation of conditional breakpoint expression failed with " ++ e ++ "\nIgnoring..."

              doEval' cond evalFailedMsg breakResume $ \ resultVal resultType -> do
                if resultType == "Bool" then do
                  if resultVal == "True" then do
                    performAction action
                  else
                    resume breakResume
                else do
                  logSDoc Logger.Warning (evalFailedMsg "\"expression resultType is != Bool\"")
                  resume breakResume
            BreakpointDisabled -> resume breakResume
            -- The counting is handled by @GHC.setupBreakpoint@
            BreakpointAfterCount _ -> performAction action
            BreakpointEnabled -> performAction action
  where
    doEval' expr evalFailedMsg br k = doEval expr >>= \case
            EvalStopped{} -> error "impossible for doEval"
            EvalCompleted { resultVal, resultType } ->
              k resultVal resultType
            EvalException { resultVal } -> do
              logSDoc Logger.Warning (evalFailedMsg resultVal)
              resume br
            EvalAbortedWith e -> do
              logSDoc Logger.Warning (evalFailedMsg e)
              resume br
    resume r = resumeExec GHC.RunToCompletion Nothing r >>= handleExecResult

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectName :: Name -> Debugger (Maybe VarInfo)
inspectName n = do
  GHC.lookupName n >>= \case
    Nothing -> do
      liftIO . putStrLn =<< display (text "Failed to lookup name: " <+> ppr n)
      pure Nothing
    Just tt -> Just <$> do
      fam_envs <- getFamInstEnvs'
      tyThingToVarInfo fam_envs tt

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectId :: Id -> Debugger (Maybe VarInfo)
inspectId (GHC.AnId -> tt) = Just <$> do
  fam_envs <- getFamInstEnvs'
  tyThingToVarInfo fam_envs tt

#if !MIN_VERSION_ghc(10,1,0)
-- | This only works because GHC's 'handleRunStatus' always pushes to the
-- resume context stack before returning the 'ExecBreak'. This works as long as
-- we consult the resume context stack immediately after the evaluation... but
-- it stops working in a multi-threaded capable debugger. When the
-- multi-threaded capabilities are released, we will no longer need this
-- function nor the `resumeContext` because `ExecBreak` now returns the
-- `Resume` directly.
getRemoteThreadIdFromContext :: Debugger RemoteThreadId
getRemoteThreadIdFromContext = do
  GHC.getResumeContext >>= \case
    resume1:_ ->
      getRemoteThreadIdFromRemoteContext $ GHC.resumeContext resume1
    _ -> error "No resumes but stopped?!?"
#endif
