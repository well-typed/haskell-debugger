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
import Data.Maybe

import GHC qualified
import GHC (
  ForeignHValue,
  InteractiveImport (..),
  mkHsString,
  nlList,
  nlHsLit,
  )
import qualified GHC.Plugins as GHC
import GHC.Unit.Types
import GHC.Driver.Monad as GHC
import qualified GHCi.Message as GHCi
import qualified GHC.Runtime.Interpreter as GHCi (evalIO)
import qualified GHC.Driver.Env as HscEnv (hscInterp)

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
import Colog.Core as Logger
import GHC.Debugger.Runtime.Thread.Resume
import GHC.Debugger.Session (getInteractiveDebuggerDynFlags)
import Data.List (find)
import GHC.Unit.Module.Graph as GHC
import GHC.Debugger.Session.Builtin (runInternal, debuggerRuntimeInternalModName)
import GHC.Runtime.Eval.Types

import GHC.Debugger.Run.Resume
import GHC.Debugger.Run.Handler
import GHC.Driver.Config.Parser
import GHC.Debugger.Run.Eval

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

-- | Run a program with debugging enabled
debugExecution :: AbsFilePath -> EntryPoint
               -> [String] {-^ Args -}
               -> Maybe AbsFilePath {-^ Working directory of the debuggee -}
               -> [(String, String)] {-^ Extra environment variables for the debuggee -}
               -> Debugger EvalResult
debugExecution entryFile entry args mcwd env = do
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
      return (apply fn args, GHC.execOptions)

  logSDoc Logger.Debug "Compiled wrapper."

  -- See Note [Debuggee working directory and environment]
  prepareDebuggeeProcess mcwd env

  exec_res <- GHC.execStmt entryExp exOpts
#ifdef GHC_HAS_MULTITHREADED_DBG
    { execIsolateMode = GHC.MultiThreadedBreaks } -- yeah!
#endif

  logSDoc Logger.Debug $ "Executed entryExp: " <+> text entryExp

  GHC.setContext old_context

  res <- handleExecResult exec_res
  logSDoc Logger.Debug $ "Computed EvalResult."
  pure res
  where
    apply x xs = unwords $ x : map (\ a -> "(" ++ a ++ ")") xs

    -- mkEvalWrapper is mostly duplicating ghci's implementation, except we
    -- reference `evalWrapper` from GHC.Debugger.Runtime.Internal (See Note [debuggerInternal unit]).
    mkEvalWrapper :: GhcMonad m => String -> [String] -> m ForeignHValue
    mkEvalWrapper progname' args' =
      runInternal $ GHC.compileParsedExprRemote
      $ evalWrapper' `GHC.mkHsApp` nlHsString progname'
                     `GHC.mkHsApp` nlList (map nlHsString args')
      where
        nlHsString = nlHsLit . mkHsString
        evalWrapper' :: GHC.LHsExpr GHC.GhcPs
        evalWrapper' =
          GHC.nlHsVar $ GHC.mkRdrQual debuggerRuntimeInternalModName (GHC.mkVarOcc "evalWrapper")

    findUnitIdOfEntryFile :: GhcMonad m => AbsFilePath -> m GHC.ModuleNodeInfo
    findUnitIdOfEntryFile afp = do
      modSums <- getAllLoadedModulesWithPaths
      case find ((== unAbs afp) . unAbs . fst) modSums of
        Nothing -> do
          let norms = map fst modSums
          error $ "findUnitIdOfEntryFile: no unit id found for: " ++ unAbs afp ++ "\nCandidates were:\n" ++ unlines (map show norms)
        Just (_,summary) -> pure summary

-- | Apply the requested working directory and environment variables in the
-- debuggee process, right before the entry point is executed.
--
-- See Note [Debuggee working directory and environment]
prepareDebuggeeProcess :: Maybe AbsFilePath -> [(String, String)] -> Debugger ()
prepareDebuggeeProcess Nothing [] = pure ()
prepareDebuggeeProcess mcwd env = do
  logSDoc Logger.Debug $ "Preparing debuggee process:" <+> text (show (fmap unAbs mcwd, env))
  interp <- HscEnv.hscInterp <$> GHC.getSession
  runInternal $ do
    -- 'show' renders valid Haskell literals, and Prelude is in scope in 'runInternal'.
    code <- GHC.compileExprRemote $ unwords
      [ GHC.moduleNameString debuggerRuntimeInternalModName ++ ".prepareDebuggee"
      , showsPrec 11 (fmap unAbs mcwd) ""
      , show env
      ]
    liftIO $ GHCi.evalIO interp code

{-
Note [Debuggee working directory and environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A launch configuration may ask for the debuggee to run in a particular working
directory ("cwd") and with extra environment variables ("env", e.g. to prepend a
directory to PATH). We apply both *inside the debuggee process*, by evaluating
'GHC.Debugger.Runtime.Internal.prepareDebuggee' right before the entry point is
executed, rather than when the debuggee process is spawned. Reasons:

1. One code path covers every way the debuggee process is created: the external
   interpreter spawned by the debugger, or spawned by the DAP client through a
   runInTerminal reverse request. We do not depend on the DAP client honouring
   the "cwd"/"env" fields of that request.

2. Loading is unaffected. GHC computes object, archive and library paths in the
   debugger process (some of them relative, e.g. from hie-bios flags) and sends
   them verbatim to the external interpreter. Starting the interpreter in a
   different directory could break those loads; changing directory only after
   loading, immediately before 'execStmt', cannot.

3. The debugger's own working directory must not change. Several places use
   'getCurrentDirectory' to make GHC source spans absolute (see #263), so
   calling 'setCurrentDirectory' in the debugger process is not an option. With
   the external interpreter the two processes are distinct and only the
   debuggee's directory changes.

4. 'setEnv' merges over the inherited environment, so the debuggee and any
   process it spawns (whose PATH lookup uses the process environment) see the
   new values.

With the internal interpreter the debuggee *is* the debugger process. Changing
its working directory would break (3), so "cwd" is rejected at launch in that
mode. Environment variables are still applied; they also become visible to the
debugger, which is harmless after session setup has finished.

Caveat: dynamic loads triggered later from the debug REPL (e.g. a package
module import needing a library found through a relative -L directory) resolve
against the new working directory. This is an edge case, and strictly better
than changing directory at spawn time.
-}

doResume :: RemoteThreadId -> ResumeStep -> ResumeTheWorld -> Debugger EvalResult
doResume tid step _stop_world = do
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
doEvalCommand :: Maybe (RemoteThreadId, Int)
              -- ^ If Just, evaluate the expression in a context where the variables
              -- from the given thread at frame index are available.
              -- If Nothing, it evaluates in the stored globalIC.
              -> String
              -> Debugger EvalResult
doEvalCommand mtid expr = do
  dflags <- getInteractiveDebuggerDynFlags
  let pflags = initParserOpts dflags
  if GHC.isStmt pflags expr
    then doEval handleExecResult mtid expr
    else addImport expr
