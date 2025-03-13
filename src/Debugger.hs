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
import qualified GHC.Runtime.Debugger as GHCD
import GHC.Runtime.Debugger.Breakpoints
import qualified GHC.Runtime.Heap.Inspect as GHCI

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

-- | Evaluate expression. Includes context of breakpoint if stopped at one (the current interactive context).
doEval :: String -> Debugger EvalResult
doEval exp = do
  -- consider always using :trace like ghci-dap to always have a stacktrace?
  -- better solution could involve profiling stack traces or from IPE info?
  GHC.execStmt exp GHC.execOptions >>= \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> error $ "Nothing bound for expression: " ++ exp
        Right (n:ns) -> inspectName n >>= \case
          Just (a, b) -> return (EvalCompleted a b)
          Nothing     -> liftIO $ fail "doEval failed"
    ExecBreak {breakNames, breakPointId} ->
      -- Probably we should only stop at breakpoints while doing "doEval" if
      -- the breakpoint is in one of the loaded modules. Otherwise continue?
      -- What about break on exception here and elsewhere?
      -- Or perhaps NEVER break in eval requests, only when continuing/running program
      error "no!"

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

