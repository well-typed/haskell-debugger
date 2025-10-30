{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module GHC.Debugger.Monad where

import Prelude hiding (mod)
import Data.Function
import System.Exit
import System.IO
import System.FilePath (normalise)
import System.Directory (makeAbsolute)
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (assert)

import Control.Monad.Catch

import GHC
import GHC.Data.Maybe (expectJust)
import qualified GHCi.BreakArray as BA
import GHC.Driver.DynFlags as GHC
import GHC.Unit.Module.ModSummary as GHC
import GHC.Utils.Outputable as GHC
import GHC.Utils.Logger as GHC
import GHC.Types.Unique.Supply as GHC
import GHC.Runtime.Loader as GHC
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Debugger.Breakpoints
import GHC.Driver.Env

import Data.IORef
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IntMap as IM

import Control.Monad.Reader
import System.Posix.Signals

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Term.Key
import GHC.Debugger.Runtime.Term.Cache
import GHC.Debugger.Session
import GHC.ByteCode.Breakpoints
import qualified GHC.Debugger.Breakpoint.Map as BM

-- | A debugger action.
newtype Debugger a = Debugger { unDebugger :: ReaderT DebuggerState GHC.Ghc a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , GHC.HasDynFlags, MonadReader DebuggerState )

-- | State required to run the debugger.
--
-- - Keep track of active breakpoints to easily unset them all.
data DebuggerState = DebuggerState
      { activeBreakpoints :: IORef (BM.BreakpointMap (BreakpointStatus, BreakpointKind))
        -- ^ Maps a 'InternalBreakpointId' in Trie representation (map of Module to map of Int) to the
        -- 'BreakpointStatus' it was activated with.

      , varReferences     :: IORef (IM.IntMap TermKey, TermKeyMap Int)
      -- ^ When we're stopped at a breakpoint, this maps variable reference to
      -- Terms to allow further inspection and forcing by reference.
      --
      -- This map is only valid while stopped in this context. After stepping
      -- or resuming evaluation in any available way, this map becomes invalid
      -- and should therefore be cleaned.
      --
      -- The TermKeyMap map is a reverse lookup map to find which references
      -- already exist for given names

      , termCache         :: IORef TermCache
      -- ^ TermCache

      , genUniq           :: IORef Int
      -- ^ Generates unique ints
      }

-- | Enabling/Disabling a breakpoint
data BreakpointStatus
      -- | Breakpoint is disabled
      --
      -- Note: this must be the first constructor s.t.
      --  @BreakpointDisabled < {BreakpointEnabled, BreakpointAfterCount}@
      = BreakpointDisabled
      -- | Breakpoint is enabled
      | BreakpointEnabled
      -- | Breakpoint is disabled the first N times and enabled afterwards
      | BreakpointAfterCount Int
      -- | Breakpoint is enabled when condition evaluates to true
      | BreakpointWhenCond String
      -- | Breakpoint is disabled the first N times the condition evaluates to
      -- true and enabled in the next time it is true
      | BreakpointAfterCountCond Int String
      deriving (Eq, Ord, Show)

instance Outputable BreakpointStatus where ppr = text . show

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | Additional settings configuring the debugger
data RunDebuggerSettings = RunDebuggerSettings
      { supportsANSIStyling :: Bool
      , supportsANSIHyperlinks :: Bool
      }

-- | Run a 'Debugger' action on a session constructed from a given GHC invocation.
runDebugger :: Handle     -- ^ The handle to which GHC's output is logged. The debuggee output is not affected by this parameter.
            -> FilePath   -- ^ Cradle root directory
            -> FilePath   -- ^ Component root directory
            -> FilePath   -- ^ The libdir (given with -B as an arg)
            -> [String]   -- ^ The list of units included in the invocation
            -> [String]   -- ^ The full ghc invocation (as constructed by hie-bios flags)
            -> FilePath   -- ^ Path to the main function
            -> RunDebuggerSettings -- ^ Other debugger run settings
            -> Debugger a -- ^ 'Debugger' action to run on the session constructed from this invocation
            -> IO a
runDebugger dbg_out rootDir compDir libdir units ghcInvocation' mainFp conf (Debugger action) = do
  let ghcInvocation = filter (\case ('-':'B':_) -> False; _ -> True) ghcInvocation'
  GHC.runGhc (Just libdir) $ do
    -- Workaround #4162
    _ <- liftIO $ installHandler sigINT Default Nothing
    dflags0 <- GHC.getSessionDynFlags
    let dflags1 = dflags0
          { GHC.ghcMode = GHC.CompManager
          , GHC.ghcLink = GHC.LinkInMemory
          , GHC.verbosity = 1
          , GHC.canUseColor = conf.supportsANSIStyling
          , GHC.canUseErrorLinks = conf.supportsANSIHyperlinks
          }
          -- Default GHCi settings
          `GHC.gopt_set` GHC.Opt_ImplicitImportQualified
          `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
          `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
          `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
          `GHC.gopt_set` GHC.Opt_InsertBreakpoints
          & setBytecodeBackend
          & enableByteCodeGeneration

    GHC.modifyLogger $
      -- Override the logger to output to the given handle
      GHC.pushLogHook (const $ debuggerLoggerAction dbg_out)

    -- TODO: this is weird, we set the session dynflags now to initialise
    -- the hsc_interp.
    -- This is incredibly dubious
    _ <- GHC.setSessionDynFlags dflags1

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins

    flagsAndTargets <- parseHomeUnitArguments mainFp compDir units ghcInvocation dflags1 rootDir
    setupHomeUnitGraph (NonEmpty.toList flagsAndTargets)

    dflags6 <- GHC.getSessionDynFlags

    -- Should this be done in GHC=
    liftIO $ GHC.initUniqSupply (GHC.initialUnique dflags6) (GHC.uniqueIncrement dflags6)

    ok_flag <- GHC.load GHC.LoadAllTargets
    when (GHC.failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

    -- TODO: Shouldn't initLoaderState be called somewhere?

    -- Set interactive context to import all loaded modules
    -- TODO: Think about Note [GHCi and local Preludes] and what is done in `getImplicitPreludeImports`
    let preludeImp = GHC.IIDecl . GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"
    mss <- getAllLoadedModules
    GHC.setContext $ preludeImp : map (GHC.IIModule . GHC.ms_mod) mss

    runReaderT action =<< initialDebuggerState


-- | The logger action used to log GHC output
debuggerLoggerAction :: Handle -> LogAction
debuggerLoggerAction h a b c d = do
  hSetEncoding h utf8 -- GHC output uses utf8
  defaultLogActionWithHandles h h a b c d

-- | Registers or deletes a breakpoint in the GHC session and from the list of
-- active breakpoints that is kept in 'DebuggerState', depending on the
-- 'BreakpointStatus' being set.
--
-- Returns @True@ when the breakpoint status is changed.
registerBreakpoint :: GHC.BreakpointId -> BreakpointStatus -> BreakpointKind -> Debugger (Bool, [GHC.InternalBreakpointId])
registerBreakpoint bp status kind = do

  -- Set breakpoint in GHC session
  let breakpoint_count = breakpointStatusInt status
  hsc_env <- GHC.getSession
  internal_break_ids <- getInternalBreaksOf bp
  changed <- forM internal_break_ids $ \ibi -> do
    GHC.setupBreakpoint (hscInterp hsc_env) ibi breakpoint_count

    -- Register breakpoint in Debugger state for every internal breakpoint
    brksMapRef <- asks activeBreakpoints
    liftIO $ atomicModifyIORef' brksMapRef $ \brksMap ->
      case status of
        -- Disabling the breakpoint:
        BreakpointDisabled ->
          (BM.delete ibi brksMap, True{-assume map always contains BP, thus changes on deletion-})

        -- Enabling the breakpoint:
        _ -> case BM.lookup ibi brksMap of
          Just (status', _kind)
            | status' == status
            -> -- Nothing changed, OK
               (brksMap, False)
          _ -> -- Else, insert
            (BM.insert ibi (status, kind) brksMap, True)

  return (any id changed, internal_break_ids)


-- | Get a list with all currently active breakpoints on the given module (by path)
--
-- If the path argument is @Nothing@, get all active function breakpoints instead
getActiveBreakpoints :: Maybe FilePath -> Debugger [GHC.InternalBreakpointId]
getActiveBreakpoints mfile = do
  bm <- asks activeBreakpoints >>= liftIO . readIORef
  case mfile of
    Just file -> do
      mms <- getModuleByPath file
      case mms of
        Right ms -> do
          hsc_env    <- getSession
          imodBreaks <- liftIO $ expectJust <$> readIModBreaksMaybe (hsc_HUG hsc_env) (ms_mod ms)
          return
            [ ibi
            | ibi <- BM.keys bm
            , getBreakSourceMod ibi imodBreaks == ms_mod ms
            -- assert: status is always > disabled
            ]
        Left e -> do
          displayWarnings [e]
          return []
    Nothing -> do
      return
        [ ibi
        | (ibi, (status, kind)) <- BM.toList bm
        -- Keep only function breakpoints in this case
        , FunctionBreakpointKind == kind
        , assert (status > BreakpointDisabled) True
        ]

-- | List all loaded modules 'ModSummary's
getAllLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getAllLoadedModules =
  (GHC.mgModSummaries <$> GHC.getModuleGraph) >>=
    filterM (\ms -> GHC.isLoadedModule (GHC.ms_unitid ms) (GHC.ms_mod_name ms))

-- | Get a 'ModSummary' of a loaded module given its 'FilePath'
getModuleByPath :: FilePath -> Debugger (Either String ModSummary)
getModuleByPath path = do
  -- do this every time as the loaded modules may have changed
  lms <- getAllLoadedModules
  absPath <- liftIO $ makeAbsolute path
  let matches ms = normalise (msHsFilePath ms) == normalise absPath
  return $ case filter matches lms of
    [x] -> Right x
    [] -> Left $ "No module matched " ++ path ++ ".\nLoaded modules:\n" ++ show (map msHsFilePath lms) ++ "\n. Perhaps you've set a breakpoint on a module that isn't loaded into the session?"
    xs -> Left $ "Too many modules (" ++ showPprUnsafe xs ++ ") matched " ++ path ++ ". Please report a bug at https://github.com/well-typed/haskell-debugger."

--------------------------------------------------------------------------------
-- Variable references
--------------------------------------------------------------------------------

-- | Find a variable's associated Term and Name by reference ('Int')
lookupVarByReference :: Int -> Debugger (Maybe TermKey)
lookupVarByReference i = do
  ioref <- asks varReferences
  (rm, _) <- readIORef ioref & liftIO
  return $ IM.lookup i rm

-- | Finds or creates an integer var reference for the given 'TermKey'.
-- TODO: Arguably, this mapping should be part of the debug-adapter, and
-- haskell-debugger should deal in 'TermKey' terms only.
getVarReference :: TermKey -> Debugger Int
getVarReference key = do
  ioref     <- asks varReferences
  (rm, tkm) <- readIORef ioref & liftIO
  (i, tkm') <- case lookupTermKeyMap key tkm of
    Nothing -> do
      new_i <- freshInt
      return (new_i, insertTermKeyMap key new_i tkm)
    Just existing_i ->
      return (existing_i, tkm)
  let rm' = IM.insert i key rm
  writeIORef ioref (rm', tkm') & liftIO
  return i

-- | Whenever we run a request that continues execution from the current
-- suspended state, such as Next,Step,Continue, this function should be called
-- to delete the variable references that become invalid as we leave the
-- suspended state.
--
-- In particular, @'varReferences'@ is reset.
--
-- See also section "Lifetime of Objects References" in the DAP specification.
leaveSuspendedState :: Debugger ()
leaveSuspendedState = do
  ioref <- asks varReferences
  liftIO $ writeIORef ioref mempty

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

defaultDepth :: Int
defaultDepth =  2 -- the depth determines how much of the runtime structure is traversed.
                  -- @obtainTerm@ and friends handle fetching arbitrarily nested data structures
                  -- so we only depth enough to get to the next level of subterms.

-- | Evaluate a suspended Term to WHNF.
--
-- Used in @'getVariables'@ to reply to a variable introspection request.
seqTerm :: Term -> Debugger Term
seqTerm term = do
  hsc_env <- GHC.getSession
  let
    interp = hscInterp hsc_env
    unit_env = hsc_unit_env hsc_env
  case term of
    Suspension{val, ty} -> liftIO $ do
      r <- GHCi.seqHValue interp unit_env val
      () <- fromEvalResult r
      let
        forceThunks = False {- whether to force the thunk subterms -}
        forceDepth  = defaultDepth
      cvObtainTerm hsc_env forceDepth forceThunks ty val
    NewtypeWrap{wrapped_term} -> do
      wrapped_term' <- seqTerm wrapped_term
      return term{wrapped_term=wrapped_term'}
    _ -> return term

-- | Evaluate a Term to NF
deepseqTerm :: Term -> Debugger Term
deepseqTerm t = case t of
  Suspension{}   -> do t' <- seqTerm t
                       deepseqTerm t'
  Term{subTerms} -> do subTerms' <- mapM deepseqTerm subTerms
                       return t{subTerms = subTerms'}
  NewtypeWrap{wrapped_term}
                 -> do wrapped_term' <- deepseqTerm wrapped_term
                       return t{wrapped_term = wrapped_term'}
  _              -> do seqTerm t

-- | Resume execution with single step mode 'RunToCompletion', skipping all breakpoints we hit, until we reach 'ExecComplete'.
--
-- We use this in 'doEval' because we want to ignore breakpoints in expressions given at the prompt.
continueToCompletion :: Debugger GHC.ExecResult
continueToCompletion = do
  execr <- GHC.resumeExec GHC.RunToCompletion Nothing
  case execr of
    GHC.ExecBreak{} -> continueToCompletion
    GHC.ExecComplete{} -> return execr

-- | Turn a 'BreakpointStatus' into its 'Int' representation for 'BreakArray'
breakpointStatusInt :: BreakpointStatus -> Int
breakpointStatusInt = \case
  BreakpointEnabled          -> BA.breakOn  -- 0
  BreakpointDisabled         -> BA.breakOff -- -1
  BreakpointAfterCount n     -> n           -- n
  BreakpointWhenCond{}       -> BA.breakOn  -- always stop, cond evaluated after
  BreakpointAfterCountCond{} -> BA.breakOn  -- ditto, decrease only when cond is true

-- | Generate a new unique 'Int'
freshInt :: Debugger Int
freshInt = do
  ioref <- asks genUniq
  i <- readIORef ioref & liftIO
  let !i' = i+1
  writeIORef ioref i'  & liftIO
  return i

-- | Initialize a 'DebuggerState'
initialDebuggerState :: GHC.Ghc DebuggerState
initialDebuggerState = DebuggerState <$> liftIO (newIORef BM.empty)
                                     <*> liftIO (newIORef mempty)
                                     <*> liftIO (newIORef mempty)
                                     <*> liftIO (newIORef 0)

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: GHC.Ghc a -> Debugger a
liftGhc = Debugger . ReaderT . const

--------------------------------------------------------------------------------

type Warning = String

displayWarnings :: [Warning] -> Debugger ()
displayWarnings = liftIO . putStrLn . unlines

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance GHC.HasLogger Debugger where
  getLogger = liftGhc GHC.getLogger

instance GHC.GhcMonad Debugger where
  getSession = liftGhc GHC.getSession
  setSession s = liftGhc $ GHC.setSession s

--------------------------------------------------------------------------------

-- | Find all the internal breakpoints that use the given source-level breakpoint id
getInternalBreaksOf :: BreakpointId -> Debugger [InternalBreakpointId]
getInternalBreaksOf bi = do
  bs <- mkBreakpointOccurrences
  return $
    fromMaybe [] {- still not found after refresh -} $
      lookupBreakpointOccurrences bs bi
