{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase #-}
module Debugger where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import System.Exit

import GHC
import GHC.Driver.Ppr as GHC
import GHC.Driver.DynFlags as GHC
import GHC.Utils.Outputable as GHC
import GHC.Unit.Module.Graph as GHC
import qualified GHC.Runtime.Debugger as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI
import GHC.Runtime.Loader as GHC
import GHC.Runtime.Eval as GHC
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List

import Debugger.Monad
import Debugger.Interface.Messages

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> Debugger Bool
setBreakpoint ModuleBreak{path, lineNum, columNum=_} = do
  mod <- getModuleByPath path
  hsc_env <- getSession
  let breakpoint_count = 0 -- how many times to skip this breakpoint?

  -- ROMES:TODO: To find bi_tick_index, look at `breakByModuleLine` in
  -- GHCi/UI.hs
  GHC.setupBreakpoint hsc_env
        BreakpointId { bi_tick_Mod = ms_mod mod
                     , bi_tick_index = }
        breakpoint_count
  return True

-- | Evaluate expression. Includes context of breakpoint if stopped at one (the current interactive context).
doEval :: String -> Debugger EvalResult
doEval exp = Debugger $ do
  -- consider always using :trace like ghci-dap to always have a stacktrace?
  -- better solution could involve profiling stack traces or from IPE info?
  GHC.execStmt exp GHC.execOptions >>= \case
    ExecComplete {execResult} -> do
      case execResult of
        Left e -> return (EvalException (show e) "SomeException")
        Right [] -> error $ "Nothing bound for expression: " ++ exp
        Right (n:ns) -> do
          Just (a, b) <- unDebugger $ inspectName n
          return (EvalCompleted a b)
    ExecBreak {breakNames, breakPointId} -> error "no!"

--------------------------------------------------------------------------------
-- Ghc utilities
--------------------------------------------------------------------------------

-- | Get the value and type of a given 'Name' as rendered strings.
inspectName :: Name -> Debugger (Maybe (String {-^ Value -}, String {-^ Type -}))
inspectName n = do
  liftGhc (GHC.lookupName n) >>= \case
    Nothing -> pure Nothing
    Just tt -> Just <$> case tt of
      t@(AConLike c) -> (,) <$> display c <*> display t
      t@(ATyCon c)   -> (,) <$> display c <*> display t
      t@(ACoAxiom c) -> (,) <$> display c <*> display t
      AnId i -> do
        term <- liftGhc $ GHC.obtainTermFromId 100{-depth-} False{- only force on request (command)-} i
        (,) <$> (display =<< liftGhc (GHCD.showTerm term)) <*> display (GHCI.termType term)

-- | Get a 'ModSummary' of a loaded module given its 'FilePath'
getModuleByPath :: FilePath -> Debugger ModSummary
getModuleByPath path = liftGhc $ do
  -- do this everytime as the loaded modules may have changed
  mg <- GHC.getModuleGraph
  let matches ms = isLoadedModSummary ms && msHsFilePath ms `List.isSuffixOf` path
  case filter matches (GHC.mgModSummaries mg) of
    [x] -> return x
    [] -> error $ "No Module matched " ++ path
    xs -> error $ "Too many modules (" ++ show xs ++ ") matched " ++ path

--------------------------------------------------------------------------------
-- General utilities
--------------------------------------------------------------------------------

-- | Display an Outputable value as a String
display :: Outputable a => a -> Debugger String
display x = Debugger $ do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: Ghc a -> Debugger a
liftGhc = Debugger
