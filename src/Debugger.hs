{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase #-}
module Debugger where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Functor

import System.Exit

import GHC
import GHC.Driver.Ppr as GHC
import GHC.Driver.DynFlags as GHC
import GHC.Utils.Outputable as GHC
import GHC.Unit.Module.Graph as GHC
import GHC.Unit.Module.ModSummary as GHC
import qualified GHC.Runtime.Debugger as GHCD
import GHC.Runtime.Debugger.Breakpoints
import qualified GHC.Runtime.Heap.Inspect as GHCI
import GHC.Runtime.Loader as GHC
import GHC.Runtime.Eval as GHC hiding (obtainTermFromId)
import qualified GHCi.BreakArray as BA

import qualified Data.List.NonEmpty as NE
import qualified Data.List as List

import Data.Array
import qualified Data.IntMap as IM

import Debugger.Monad
import Debugger.Interface.Messages

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> Debugger Bool
setBreakpoint ModuleBreak{path, lineNum, columnNum} = do
  hsc_env <- getSession
  let breakpoint_count = BA.breakOn -- enable bp.

  mod <- getModuleByPath path

  mticks <- makeModuleLineMap (ms_mod mod)
  let mbid = fst <$> do
        ticks <- mticks
        case columnNum of
          Nothing -> findBreakByLine lineNum ticks
          Just col -> findBreakByCoord (lineNum, col) ticks

  case mbid of
    Nothing -> do
      liftIO $ putStrLn "TODO: HOW TO REPORT ERROR"
      return False
    Just bid -> do
      GHC.setupBreakpoint hsc_env
          BreakpointId { bi_tick_mod = ms_mod mod
                       , bi_tick_index = bid }
          breakpoint_count
      return True

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
  mg <- GHC.getModuleGraph
  let matches ms = GHC.isLoadedModule (ms_unitid ms) (ms_mod_name ms)
                    <&> (&& msHsFilePath ms `List.isSuffixOf` path)
  filterM matches (GHC.mgModSummaries mg) >>= \case
    [x] -> return x
    [] -> error $ "No Module matched " ++ path
    xs -> error $ "Too many modules (" ++ showPprUnsafe xs ++ ") matched " ++ path

--------------------------------------------------------------------------------
-- General utilities
--------------------------------------------------------------------------------

-- | Display an Outputable value as a String
display :: Outputable a => a -> Debugger String
display x = liftGhc $ do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: Ghc a -> Debugger a
liftGhc = Debugger
