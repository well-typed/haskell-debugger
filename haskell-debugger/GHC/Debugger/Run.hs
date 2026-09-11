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
import Control.Monad.Catch
import Data.Maybe

import GHC qualified
import GHC (
  ForeignHValue,
  InteractiveImport (..),
  mkHsString,
  nlHsLit,
  nlList,
  parseImportDecl,
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
import GHC.Driver.Monad as GHC
import GHC.Driver.Env as GHC
import qualified GHC.Driver.Config.Parser as GHC
import GHC.Types.Name.Occurrence (mkVarOccFS)
import GHC.Types.Name.Reader as RdrName (mkOrig)
import qualified GHCi.Message as GHCi

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import Colog.Core as Logger
import GHC.Debugger.Runtime.Thread.Resume
import GHC.Debugger.Session (getInteractiveDebuggerDynFlags)
import Data.List (find)
import GHC.Unit.Module.Graph as GHC
import GHC.Runtime.Eval.Types

import GHC.Debugger.Run.Resume
import GHC.Debugger.Run.Handler
import GHC.Debugger.Run.Eval

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
  ss     <- resolveResumeStep resume step
  -- TODO: stop_world toggles global hit breakpoints mode (todo: look at mode
  -- to determine how to stop) and here run all the threads accordingly.
  -- TODO: for the first iteration, simply call pause on all threads (except
  -- for those with specific debugger labels); but pause will only set step_in,
  -- and won't be able to pause compiled threads, but that is follow up work.
  resumeExec ss Nothing resume
    >>= handleExecResult

-- | Generalized `doEval` that also handles `imports`
doEvalCommand :: String -> Debugger EvalResult
doEvalCommand expr = do
  dflags <- getInteractiveDebuggerDynFlags
  let pflags = GHC.initParserOpts dflags
  if GHC.isStmt pflags expr
    then doEval handleExecResult _ expr
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
