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
module GHC.Debugger.Evaluation where

import Control.Monad.IO.Class
import Control.Monad.Catch
import qualified Data.List as List
import Data.Maybe
import System.FilePath
import qualified Prettyprinter as Pretty

import GHC
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
import GHC.Unit.Types
import GHC.Data.FastString
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Monad as GHC
import GHC.Driver.Env as GHC
import GHC.Runtime.Debugger.Breakpoints as GHC
import qualified GHC.Unit.Module.ModSummary as GHC
import GHC.Types.Name.Occurrence (mkVarOccFS)
import GHC.Types.Name.Reader as RdrName (mkOrig)
import GHC.Utils.Outputable as GHC
import qualified GHCi.Message as GHCi
import qualified GHC.Data.Strict as Strict

import GHC.Debugger.Stopped.Variables
import GHC.Debugger.Monad
import GHC.Debugger.Utils
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Logger

data EvalLog
  = LogEvalModule GHC.Module

instance Pretty EvalLog where
  pretty = \ case
    LogEvalModule modl -> "Eval Module Context:" Pretty.<+> pretty (GHC.showSDocUnsafe (ppr modl))

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

-- | Run a program with debugging enabled
debugExecution :: Recorder (WithSeverity EvalLog) -> FilePath -> EntryPoint -> [String] {-^ Args -} -> Debugger EvalResult
debugExecution recorder entryFile entry args = do

  -- consider always using :trace like ghci-dap to always have a stacktrace?
  -- better solution could involve profiling stack traces or from IPE info?
  modSummaryOfEntryFile <- findUnitIdOfEntryFile entryFile
  let modOfEntryFile = GHC.ms_mod modSummaryOfEntryFile
      unitIdOfEntryFile = GHC.ms_unitid modSummaryOfEntryFile

  let
    evalModule = mkModule (RealUnit (Definite unitIdOfEntryFile))
                                         (moduleName modOfEntryFile)

  logWith recorder Info $ LogEvalModule evalModule
  old_context <- GHC.getContext
  GHC.setContext [GHC.IIModule evalModule]

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

  exec_res <- GHC.execStmt entryExp exOpts
  GHC.setContext old_context -- restore context after running `main`
  handleExecResult exec_res
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

    findUnitIdOfEntryFile :: GhcMonad m => FilePath -> m GHC.ModSummary
    findUnitIdOfEntryFile fp = do
      modSums <- getAllLoadedModules
      case List.find ((Just fp ==) . fmap normalise . GHC.ml_hs_file . GHC.ms_location ) modSums of
        Nothing -> error $ "findUnitIdOfEntryFile: no unit id found for: " ++ fp
        Just summary -> pure summary

-- | Resume execution of the stopped debuggee program
doContinue :: Debugger EvalResult
doContinue = do
  leaveSuspendedState
  GHC.resumeExec RunToCompletion Nothing
    >>= handleExecResult

-- | Resume execution but only take a single step.
doSingleStep :: Debugger EvalResult
doSingleStep = do
  leaveSuspendedState
  GHC.resumeExec SingleStep Nothing
    >>= handleExecResult

doStepOut :: Debugger EvalResult
doStepOut = do
  leaveSuspendedState
  mb_span <- getCurrentBreakSpan
  case mb_span of
    Nothing ->
      GHC.resumeExec (GHC.StepOut Nothing) Nothing
        >>= handleExecResult
    Just loc -> do
      md <- fromMaybe (error "doStepOut") <$> getCurrentBreakModule
      ticks <- fromMaybe (error "doLocalStep:getTicks") <$> makeModuleLineMap md
      let current_toplevel_decl = enclosingTickSpan ticks loc
      GHC.resumeExec (GHC.StepOut (Just (RealSrcSpan current_toplevel_decl Strict.Nothing))) Nothing
        >>= handleExecResult

-- | Resume execution but stop at the next tick within the same function.
--
-- To do a local step, we get the SrcSpan of the current suspension state and
-- get its 'enclosingTickSpan' to use as a filter for breakpoints in the call
-- to 'resumeExec'. Execution will only stop at breakpoints whose span matches
-- this enclosing span.
doLocalStep :: Debugger EvalResult
doLocalStep = do
  leaveSuspendedState
  mb_span <- getCurrentBreakSpan
  case mb_span of
    Nothing -> error "not stopped at a breakpoint?!"
    Just (UnhelpfulSpan _) -> do
      liftIO $ putStrLn "Stopped at an exception. Forcing step into..."
      GHC.resumeExec SingleStep Nothing >>= handleExecResult
    Just loc -> do
      md <- fromMaybe (error "doLocalStep") <$> getCurrentBreakModule
      -- TODO: Cache moduleLineMap.
      ticks <- fromMaybe (error "doLocalStep:getTicks") <$> makeModuleLineMap md
      let current_toplevel_decl = enclosingTickSpan ticks loc
      GHC.resumeExec (LocalStep (RealSrcSpan current_toplevel_decl mempty)) Nothing >>= handleExecResult

-- | Evaluate expression. Includes context of breakpoint if stopped at one (the current interactive context).
doEval :: String -> Debugger EvalResult
doEval expr = do
  excr <- (Right <$> GHC.execStmt expr GHC.execOptions) `catch` \(e::SomeException) -> pure (Left (displayException e))
  case excr of
    Left err -> pure $ EvalAbortedWith err
    Right ExecBreak{} -> continueToCompletion >>= handleExecResult
    Right r@ExecComplete{} -> handleExecResult r

-- | Turn a GHC's 'ExecResult' into an 'EvalResult' response
handleExecResult :: GHC.ExecResult -> Debugger EvalResult
handleExecResult = \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> return (EvalCompleted "" "") -- Evaluation completed without binding any result.
        Right (n:_ns) -> inspectName n >>= \case
          Just VarInfo{varValue, varType} -> return (EvalCompleted varValue varType)
          Nothing     -> liftIO $ fail "doEval failed"
    ExecBreak {breakNames = _, breakPointId = Nothing} ->
      -- Stopped at an exception
      -- TODO: force the exception to display string with Backtrace?
      return EvalStopped{breakId = Nothing}
    ExecBreak {breakNames = _, breakPointId} ->
      return EvalStopped{breakId = breakPointId}

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectName :: Name -> Debugger (Maybe VarInfo)
inspectName n = do
  GHC.lookupName n >>= \case
    Nothing -> do
      liftIO . putStrLn =<< display (text "Failed to lookup name: " <+> ppr n)
      pure Nothing
    Just tt -> Just <$> tyThingToVarInfo tt

