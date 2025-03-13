{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase #-}
module Debugger where

import Control.Monad
import Control.Monad.IO.Class

import GHC
import GHC.Driver.Ppr as GHC
import GHC.Driver.DynFlags as GHC
import GHC.Utils.Outputable as GHC
import GHC.Unit.Module.ModSummary as GHC
import GHC.Unit.Module.Env as GHC
import GHC.Data.FastString
import qualified GHC.Runtime.Debugger as GHCD
import GHC.Runtime.Debugger.Breakpoints
import GHC.Types.Name.Reader as RdrName (mkOrig)
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
import GHC.Types.Name.Occurrence
import GHC.Driver.Monad
import GHC.Driver.Env
import qualified GHC.Runtime.Heap.Inspect as GHCI
import qualified GHCi.Message as GHCi

import Data.Maybe
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List
import Data.IORef

import Data.Array
import qualified Data.IntMap as IM

import Debugger.Monad
import Debugger.Interface.Messages

-- | Remove all breakpoints set on loaded modules
clearBreakpoints :: Debugger ()
clearBreakpoints = do
  -- It would be simpler to go to all loaded modules and disable all
  -- breakpoints for that module rather than keeping track,
  -- but much less efficient at scale.
  hsc_env <- getSession
  bids <- getActiveBreakpoints
  forM_ bids $ \bid -> do
    GHC.setupBreakpoint hsc_env bid (breakpointStatusInt BreakpointDisabled)

  -- Clear out the state
  bpsRef <- asks activeBreakpoints
  liftIO $ writeIORef bpsRef emptyModuleEnv

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> BreakpointStatus -> Debugger Bool
setBreakpoint ModuleBreak{path, lineNum, columnNum} bp_status = do
  hsc_env <- getSession
  let breakpoint_count = breakpointStatusInt bp_status
  mod <- getModuleByPath path

  mticks <- makeModuleLineMap (ms_mod mod)
  let mbid = fst <$> do
        ticks <- mticks
        case columnNum of
          Nothing -> findBreakByLine lineNum ticks
          Just col -> findBreakByCoord (lineNum, col) ticks

  case mbid of
    Nothing -> do
      liftIO $ putStrLn "todo: Reply saying breakpoint was not set because the line doesn't exist."
      return False
    Just bix -> do
      let bid = BreakpointId { bi_tick_mod = ms_mod mod
                             , bi_tick_index = bix }
      GHC.setupBreakpoint hsc_env bid breakpoint_count
      registerBreakpoint bid bp_status


-- | Run a program with debugging enabled
debugExecution :: EntryPoint -> [String] {-^ Args -} -> Debugger EvalResult
debugExecution entry args = do

  -- consider always using :trace like ghci-dap to always have a stacktrace?
  -- better solution could involve profiling stack traces or from IPE info?

  (entryExp, exOpts) <- case entry of

    MainEntry nm -> do
      let prog = fromMaybe "main" nm
      wrapper <- mkEvalWrapper prog args -- bit weird that the prog name is the expression but fine
      let execWrap' fhv = GHCi.EvalApp (GHCi.EvalThis wrapper) (GHCi.EvalThis fhv)
          opts = GHC.execOptions {execWrap = execWrap'}
      return (prog, opts)

    FunctionEntry fn ->
      return (fn ++ " " ++ unwords args, GHC.execOptions)

  GHC.execStmt entryExp exOpts >>= handleExecResult

  where
    -- It's not ideal to duplicate these two functions from ghci, but its unclear where they would better live. Perhaps next to compileParsedExprRemote? The issue is run
    mkEvalWrapper :: GhcMonad m => String -> [String] ->  m ForeignHValue
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


-- | Resume execution of the stopped debuggee program
doContinue :: Debugger EvalResult
doContinue = GHC.resumeExec RunToCompletion Nothing >>= handleExecResult

-- | Resume execution but only take a single step.
doSingleStep :: Debugger EvalResult
doSingleStep = GHC.resumeExec SingleStep Nothing >>= handleExecResult

-- | Resume execution but stop at the next tick within the same function.
doLocalStep :: Debugger EvalResult
doLocalStep = do
  -- TODO: Get the resume Ctxt to read the current SrcSpan we are currently stopped at
  GHC.resumeExec (LocalStep $ error "TODO:doLocalStep") Nothing >>= handleExecResult

-- | Evaluate expression. Includes context of breakpoint if stopped at one (the current interactive context).
doEval :: String -> Debugger EvalResult
doEval exp = do
  excr <- GHC.execStmt exp GHC.execOptions
  case excr of
    ExecBreak{} -> continueToCompletion >>= handleExecResult
    ExecComplete{} -> handleExecResult excr

-- | Turn a GHC's 'ExecResult' into an 'EvalResult' response
handleExecResult :: GHC.ExecResult -> Debugger EvalResult
handleExecResult = \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> error $ "Nothing bound for expression"
        Right (n:ns) -> inspectName n >>= \case
          Just (a, b) -> return (EvalCompleted a b)
          Nothing     -> liftIO $ fail "doEval failed"
    ExecBreak {breakNames, breakPointId} ->
      undefined

-- | Resume execution with single step mode 'RunToCompletion', skipping all breakpoints we hit, until we reach 'ExecComplete'.
--
-- We use this in 'doEval' because we want to ignore breakpoints in expressions given at the prompt.
continueToCompletion :: Debugger ExecResult
continueToCompletion = do
  execr <- GHC.resumeExec RunToCompletion Nothing
  case execr of
    ExecBreak{} -> continueToCompletion
    ExecComplete{} -> return execr

--------------------------------------------------------------------------------
-- Ghc utilities
--------------------------------------------------------------------------------

-- | Get the value and type of a given 'Name' as rendered strings.
inspectName :: Name -> Debugger (Maybe (String {-^ Value -}, String {-^ Type -}))
inspectName n = do
  GHC.lookupName n >>= \case
    Nothing -> pure Nothing
    Just tt -> Just <$> case tt of
      t@(AConLike c) -> (,) <$> display c <*> display t
      t@(ATyCon c)   -> (,) <$> display c <*> display t
      t@(ACoAxiom c) -> (,) <$> display c <*> display t
      AnId i -> do
        term <- GHC.obtainTermFromId 100{-depth-} False{- only force on request (command)-} i
        (,) <$> (display =<< GHCD.showTerm term) <*> display (GHCI.termType term)

-- | Get a 'ModSummary' of a loaded module given its 'FilePath'
getModuleByPath :: FilePath -> Debugger ModSummary
getModuleByPath path = do
  -- do this everytime as the loaded modules may have changed
  lms <- getAllLoadedModules
  let matches ms = msHsFilePath ms `List.isSuffixOf` path
  case filter matches lms of
    [x] -> return x
    [] -> error $ "No Module matched " ++ path
    xs -> error $ "Too many modules (" ++ showPprUnsafe xs ++ ") matched " ++ path

-- | List all loaded modules 'ModSummary's
getAllLoadedModules :: Debugger [ModSummary]
getAllLoadedModules =
  (GHC.mgModSummaries <$> GHC.getModuleGraph) >>=
    filterM (\ms -> GHC.isLoadedModule (ms_unitid ms) (ms_mod_name ms))

--------------------------------------------------------------------------------
-- General utilities
--------------------------------------------------------------------------------

-- | Display an Outputable value as a String
display :: Outputable a => a -> Debugger String
display x = do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

