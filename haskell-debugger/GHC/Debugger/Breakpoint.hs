{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables, BangPatterns #-}
module GHC.Debugger.Breakpoint where

import Prelude hiding ((<>))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bits (xor)
import Data.IORef
import System.Directory
import System.FilePath

import GHC
import GHC.ByteCode.Breakpoints
import GHC.Data.Maybe
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env
import GHC.Driver.Ppr as GHC
import GHC.Runtime.Interpreter
import GHC.Runtime.Debugger.Breakpoints as GHC
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error (logOutput)
import GHC.Utils.Outputable as GHC
import qualified GHCi.BreakArray as BA

import GHC.Debugger.Monad
import GHC.Debugger.Session
import GHC.Debugger.Utils
import GHC.Debugger.Interface.Messages
import qualified GHC.Debugger.Breakpoint.Map as BM

--------------------------------------------------------------------------------
-- * Breakpoints
--------------------------------------------------------------------------------

-- | Remove all module breakpoints set on the given loaded module by path
--
-- If the argument is @Nothing@, clear all function breakpoints instead.
clearBreakpoints :: Maybe FilePath -> Debugger ()
clearBreakpoints mfile = do
  -- It would be simpler to go to all loaded modules and disable all
  -- breakpoints for that module rather than keeping track,
  -- but much less efficient at scale.
  hsc_env <- getSession
  bpsRef <- asks activeBreakpoints
  bids <- getActiveBreakpoints mfile
  forM_ bids $ \bid -> do
    GHC.setupBreakpoint (hscInterp hsc_env) bid (breakpointStatusInt BreakpointDisabled)
    -- Clear out from the state
    liftIO $ modifyIORef bpsRef (BM.delete bid)

getBreakpointsAt :: Breakpoint -> Debugger (Maybe SourceSpan)
getBreakpointsAt ModuleBreak{path, lineNum, columnNum} = do
  mmodl <- getModuleByPath path
  case mmodl of
    Left e -> do
      displayWarnings [e]
      return Nothing
    Right modl -> do
      mbfnd <- findBreakpoint modl lineNum columnNum
      return $ realSrcSpanToSourceSpan . snd <$> mbfnd
getBreakpointsAt _ = error "unexpected getbreakpoints without ModuleBreak"

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> BreakpointStatus -> Debugger BreakFound
setBreakpoint bp BreakpointAfterCountCond{} = do
  displayWarnings [text $ "Setting a hit count condition on a conditional breakpoint is not yet supported. Ignoring breakpoint " ++ show bp]
  return BreakNotFound
setBreakpoint ModuleBreak{path, lineNum, columnNum} bp_status = do
  mmodl <- getModuleByPath path
  case mmodl of
    Left e -> do
      displayWarnings [e]
      return BreakNotFound
    Right modl -> do
      findBreakpoint modl lineNum columnNum >>= \case
        Nothing -> return BreakNotFound
        Just (bix, spn) -> do
          let bid = BreakpointId { bi_tick_mod = ms_mod modl
                                 , bi_tick_index = bix }
          (changed, ibis) <- registerBreakpoint bid bp_status ModuleBreakpointKind
          return $ BreakFound
            { changed = changed
            , sourceSpan = realSrcSpanToSourceSpan spn
            , breakId = ibis
            }
setBreakpoint FunctionBreak{function} bp_status = do
  logger <- getLogger
  resolveFunctionBreakpoint function >>= \case
    Left e -> do
      liftIO $ logOutput logger $ text $
        "Failed to resolve function breakpoint " ++ function ++ ".\n" ++ showPprUnsafe e ++ "\nIgnoring..."
      return BreakNotFound
    Right (modl, mod_info, fun_str) -> do
      let modBreaks = GHC.modInfoModBreaks mod_info
          applyBreak (bix, spn) = do
            let bid = BreakpointId { bi_tick_mod = modl
                                   , bi_tick_index = bix }
            (changed, ibis) <- registerBreakpoint bid bp_status FunctionBreakpointKind
            return $ BreakFound
              { changed = changed
              , sourceSpan = realSrcSpanToSourceSpan spn
              , breakId = ibis
              }
      case maybe [] (findBreakForBind fun_str . imodBreaks_modBreaks) modBreaks of
        []  -> do
          liftIO $ logOutput logger (text $ "No breakpoint found by name " ++ function ++ ". Ignoring...")
          return BreakNotFound
        [b] -> applyBreak b
        bs  -> do
          liftIO $ logOutput logger (text $ "Ambiguous breakpoint found by name " ++ function ++ ": " ++ show bs ++ ". Setting breakpoints in all...")
          ManyBreaksFound <$> mapM applyBreak bs
setBreakpoint exception_bp bp_status = do
  let ch_opt | BreakpointDisabled <- bp_status
             = gopt_unset
             | otherwise
             = gopt_set
      opt | OnUncaughtExceptionsBreak <- exception_bp
          = Opt_BreakOnError
          | OnExceptionsBreak <- exception_bp
          = Opt_BreakOnException
  dflags <- getInteractiveDebuggerDynFlags
  let
    -- changed if option is ON and bp is OFF (breakpoint disabled), or if
    -- option is OFF and bp is ON (i.e. XOR)
    breakOn = bp_status /= BreakpointDisabled
    didChange = gopt opt dflags `xor` breakOn
  setInteractiveDebuggerDynFlags $ dflags `ch_opt` opt
  return (BreakFoundNoLoc didChange)

--------------------------------------------------------------------------------
-- * Lower-level interface
--------------------------------------------------------------------------------

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

-- | Turn a 'BreakpointStatus' into its 'Int' representation for 'BreakArray'
breakpointStatusInt :: BreakpointStatus -> Int
breakpointStatusInt = \case
  BreakpointEnabled          -> BA.breakOn  -- 0
  BreakpointDisabled         -> BA.breakOff -- -1
  BreakpointAfterCount n     -> n           -- n
  BreakpointWhenCond{}       -> BA.breakOn  -- always stop, cond evaluated after
  BreakpointAfterCountCond{} -> BA.breakOn  -- ditto, decrease only when cond is true

-- | Find all the internal breakpoints that use the given source-level breakpoint id
getInternalBreaksOf :: BreakpointId -> Debugger [InternalBreakpointId]
getInternalBreaksOf bi = do
  bs <- mkBreakpointOccurrences
  return $
    fromMaybe [] {- still not found after refresh -} $
      lookupBreakpointOccurrences bs bi

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Turn a @hitCount :: Maybe Int@ and @condition :: Maybe Text@ into an enabled @BreakpointStatus@.
condBreakEnableStatus :: Maybe Int {-^ hitCount -} -> Maybe String {-^ condition -} -> BreakpointStatus
condBreakEnableStatus hitCount condition = do
  case (hitCount, condition) of
    (Nothing, Nothing) -> BreakpointEnabled
    (Just i,  Nothing) -> BreakpointAfterCount i
    (Nothing, Just c)  -> BreakpointWhenCond c
    (Just i,  Just c)  -> BreakpointAfterCountCond i c

-- | Get a 'ModSummary' of a loaded module given its 'FilePath'
getModuleByPath :: FilePath -> Debugger (Either Warning ModSummary)
getModuleByPath path = do
  -- get all loaded modules this every time as the loaded modules may have changed
  lms <- getAllLoadedModules
  absPath <- liftIO $ makeAbsolute path
  let matches ms = normalise (msHsFilePath ms) == normalise absPath
  return $ case filter matches lms of
    [x] -> Right x
    [] -> Left $ text "No module matched" <+> text path <> text "."
               $$ text "Loaded modules:"
               $$ vcat (map (text . msHsFilePath) lms)
               $$ text "Perhaps you've set a breakpoint on a module that isn't loaded into the session?"
    xs -> Left $ text "Too many modules (" <> ppr xs <> text ") matched" <+> text path
              <> text ". Please report a bug at https://github.com/well-typed/haskell-debugger."

-- | Find a 'BreakpointId' index and its span from a module + line + column.
--
-- Used by 'setBreakpoints' and 'GetBreakpointsAt' requests
findBreakpoint :: ModSummary {-^ module -} -> Int {-^ line num -} -> Maybe Int {-^ column num -} -> Debugger (Maybe (Int, RealSrcSpan))
findBreakpoint modl lineNum columnNum = do
  -- TODO: Cache moduleLineMap?
  mticks <- makeModuleLineMap (ms_mod modl)
  let mbid = do
        ticks <- mticks
        case columnNum of
          Nothing -> findBreakByLine lineNum ticks
          Just col -> findBreakByCoord (lineNum, col) ticks
  return mbid
