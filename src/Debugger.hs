{-# LANGUAGE CPP, NamedFieldPuns, TupleSections, LambdaCase,
   DuplicateRecordFields, RecordWildCards, TupleSections, ViewPatterns,
   TypeApplications, ScopedTypeVariables #-}
module Debugger where

import Prelude hiding (exp, span)
import System.Exit
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Bits (xor)

import GHC
import GHC.Types.FieldLabel
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS, mkUnboundName)
import GHC.Data.FastString
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env as GHC
import GHC.Driver.Monad
import GHC.Driver.Ppr as GHC
import GHC.Runtime.Debugger.Breakpoints
import GHC.Runtime.Eval.Types as GHC
import GHC.Runtime.Eval
import GHC.Core.DataCon
import GHC.Types.Breakpoint
import GHC.Types.Id as GHC
import GHC.Types.Name.Occurrence (mkVarOcc, mkVarOccFS)
import GHC.Types.Name.Reader as RdrName (mkOrig, globalRdrEnvElts, greName)
import GHC.Types.SrcLoc
import GHC.Tc.Utils.TcType
import GHC.Unit.Module.Env as GHC
import GHC.Utils.Outputable as GHC
import GHC.Utils.Misc (zipEqual)
import qualified GHC.Runtime.Debugger as GHCD
import qualified GHC.Runtime.Heap.Inspect as GHCI
import qualified GHCi.Message as GHCi
import qualified GHC.Unit.Home.Graph as HUG

import Data.Maybe
import Control.Monad.Reader
import Data.IORef

import Debugger.Monad
import Debugger.Interface.Messages

--------------------------------------------------------------------------------
-- * Executing commands
--------------------------------------------------------------------------------

-- | Execute the given debugger command in the current 'Debugger' session
execute :: Command -> Debugger Response
execute = \case
  ClearFunctionBreakpoints -> DidClearBreakpoints <$ clearBreakpoints Nothing
  ClearModBreakpoints fp -> DidClearBreakpoints <$ clearBreakpoints (Just fp)
  SetBreakpoint bp -> DidSetBreakpoint <$> setBreakpoint bp BreakpointEnabled
  DelBreakpoint bp -> DidRemoveBreakpoint <$> setBreakpoint bp BreakpointDisabled
  GetBreakpointsAt ModuleBreak{path, lineNum, columnNum} -> do
    modl <- getModuleByPath path
    mbfnd <- getBreakpointsAt modl lineNum columnNum
    return $
      DidGetBreakpoints (realSrcSpanToSourceSpan . snd <$> mbfnd)
  GetBreakpointsAt _ -> error "unexpected getbreakpoints without ModuleBreak"
  GetStacktrace -> GotStacktrace <$> getStacktrace
  GetScopes -> GotScopes <$> getScopes
  GetVariables kind -> GotVariables <$> getVariables kind
  DoEval exp_s -> DidEval <$> doEval exp_s
  DoContinue -> DidContinue <$> doContinue
  DoSingleStep -> DidStep <$> doSingleStep
  DoStepLocal -> DidStep <$> doLocalStep
  DebugExecution { entryPoint, runArgs } -> DidExec <$> debugExecution entryPoint runArgs
  TerminateProcess -> liftIO $ do
    -- Terminate!
    putStrLn "Goodbye..."
    exitWith ExitSuccess

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
  bids <- getActiveBreakpoints mfile
  forM_ bids $ \bid -> do
    GHC.setupBreakpoint hsc_env bid (breakpointStatusInt BreakpointDisabled)

  -- Clear out the state
  bpsRef <- asks activeBreakpoints
  liftIO $ writeIORef bpsRef emptyModuleEnv

-- | Find a 'BreakpointId' and its span from a module + line + column.
--
-- Used by 'setBreakpoints' and 'GetBreakpointsAt' requests
getBreakpointsAt :: ModSummary {-^ module -} -> Int {-^ line num -} -> Maybe Int {-^ column num -} -> Debugger (Maybe (BreakIndex, RealSrcSpan))
getBreakpointsAt modl lineNum columnNum = do
  -- TODO: Cache moduleLineMap.
  mticks <- makeModuleLineMap (ms_mod modl)
  let mbid = do
        ticks <- mticks
        case columnNum of
          Nothing -> findBreakByLine lineNum ticks
          Just col -> findBreakByCoord (lineNum, col) ticks
  return mbid

-- | Set a breakpoint in this session
setBreakpoint :: Breakpoint -> BreakpointStatus -> Debugger BreakFound
setBreakpoint ModuleBreak{path, lineNum, columnNum} bp_status = do
  modl <- getModuleByPath path

  mbid <- getBreakpointsAt modl lineNum columnNum

  case mbid of
    Nothing -> return BreakNotFound
    Just (bix, span) -> do
      let bid = BreakpointId { bi_tick_mod = ms_mod modl
                             , bi_tick_index = bix }
      changed <- registerBreakpoint bid bp_status ModuleBreakpointKind
      return $ BreakFound
        { changed = changed
        , sourceSpan = realSrcSpanToSourceSpan span
        , breakId = bid
        }
setBreakpoint FunctionBreak{function} bp_status = do
  resolveFunctionBreakpoint function >>= \case
    Left e -> error (showPprUnsafe e)
    Right (modl, mod_info, fun_str) -> do
      let modBreaks = GHC.modInfoModBreaks mod_info
      case findBreakForBind fun_str modBreaks of
        [] -> error ("No breakpoint found by name " ++ function)
        [(bix, span)] -> do
          let bid = BreakpointId { bi_tick_mod = modl
                                 , bi_tick_index = bix }
          changed <- registerBreakpoint bid bp_status FunctionBreakpointKind
          return $ BreakFound
            { changed = changed
            , sourceSpan = realSrcSpanToSourceSpan span
            , breakId = bid
            }
        xs -> error ("Ambiguous breakpoint found by name " ++ function ++ ": " ++ show xs)
setBreakpoint exception_bp bp_status = do
  let ch_opt | BreakpointDisabled <- bp_status
             = gopt_unset
             | otherwise
             = gopt_set
      opt | OnUncaughtExceptionsBreak <- exception_bp
          = Opt_BreakOnError
          | OnExceptionsBreak <- exception_bp
          = Opt_BreakOnException
  dflags <- GHC.getInteractiveDynFlags
  let
    -- changed if option is ON and bp is OFF (breakpoint disabled), or if
    -- option is OFF and bp is ON (i.e. XOR)
    breakOn = bp_status /= BreakpointDisabled
    didChange = gopt opt dflags `xor` breakOn
  GHC.setInteractiveDynFlags $ dflags `ch_opt` opt
  return (BreakFoundNoLoc didChange)

--------------------------------------------------------------------------------
-- * Evaluation
--------------------------------------------------------------------------------

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
      -- TODO: if "args" is unescaped (e.g. "some", "thing"), then "some" and
      -- "thing" will be interpreted as variables. To pass strings it needs to
      -- be "\"some\"" "\"things\"".
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
doEval exp = do
  excr <- (Right <$> GHC.execStmt exp GHC.execOptions) `catch` \(e::SomeException) -> pure (Left (displayException e))
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
      return EvalStopped{breakId = toBreakpointId <$> breakPointId}

--------------------------------------------------------------------------------
-- * Stack trace
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getStacktrace :: Debugger [StackFrame]
getStacktrace = do
  topStackFrame <- GHC.getResumeContext >>= \case
    [] -> error "not stopped at a breakpoint?!"
    r:_ -> do
      case (srcSpanToRealSrcSpan $ GHC.resumeSpan r) of
        Just ss -> do
          return $ Just StackFrame {
            name = GHC.resumeDecl r
          , sourceSpan = realSrcSpanToSourceSpan ss
          }
        Nothing ->
          -- No resume span; which should mean we're stopped on an exception
          return Nothing

  return $ catMaybes [topStackFrame]

--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Get the stack frames at the point we're stopped at
getScopes :: Debugger [ScopeInfo]
getScopes = do
  GHC.getResumeContext >>= \case
    [] -> error "not stopped at a breakpoint?!"
    r:_ -> case (srcSpanToRealSrcSpan $ GHC.resumeSpan r) of
      Just rss -> do
        let sourceSpan = realSrcSpanToSourceSpan rss
        return
          [ ScopeInfo { kind = LocalVariablesScope
                      , expensive = False
                      , numVars = Nothing
                      , sourceSpan
                      }
          , ScopeInfo { kind = ModuleVariablesScope
                      , expensive = True
                      , numVars = Nothing
                      , sourceSpan
                      }
          , ScopeInfo { kind = GlobalVariablesScope
                      , expensive = True
                      , numVars = Nothing
                      , sourceSpan
                      }
          ]
      Nothing ->
        -- No resume span; which should mean we're stopped on an exception
        -- TODO: Use exception context to create source span, or at least
        -- return the source span null to have Scopes at least.
        return []

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------
-- Note [Variables Requests]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- We can receive a Variables request for three different reasons
--
-- 1. To get the variables in a certain scope
-- 2. To inspect the value of a lazy variable
-- 3. To expand the structure of a variable
--
-- The replies are, respectively:
--
-- (VARR)
-- (a) All the variables in the request scope
-- (b) ONLY the variable requested
-- (c) The fields of the variable requested but NOT the original variable

-- | Get variables using a variable/variables reference
--
-- If the Variable Request ends up being case (VARR)(b), then we signal the
-- request forced the variable and return @Left varInfo@. Otherwise, @Right vis@.
--
-- See Note [Variables Requests]
getVariables :: VariableReference -> Debugger (Either VarInfo [VarInfo])
getVariables vk = do
  hsc_env <- getSession
  GHC.getResumeContext >>= \case
    [] -> error "not stopped at a breakpoint?!"
    r:_ -> case vk of

      -- (VARR)(b,c)
      SpecificVariable i -> do
        -- Only force thing when scrutinizing specific variable
        lookupVarByReference i >>= \case
          Nothing -> error "lookupVarByReference failed"
          Just (n, term) -> do
            term' <- seqTerm term
            vi <- termToVarInfo n term'
            case term of

              -- (VARR)(b)
              Suspension{} -> do
                -- Original Term is a suspension:
                -- It is a "lazy" DAP variable, so our reply can ONLY include
                -- this single variable. So we erase the @varFields@ after the fact.
                return (Left vi{varFields = NoFields})

              -- (VARR)(c)
              _ -> Right <$> do
                -- Original Term was already something other than a Suspension;
                -- Meaning the @SpecificVariable@ request means to inspect the structure.
                -- Return ONLY the fields
                case varFields vi of
                  NoFields -> return []
                  LabeledFields xs -> return xs
                  IndexedFields xs -> return xs

      -- (VARR)(a) from here onwards

      LocalVariables -> fmap Right $
        -- bindLocalsAtBreakpoint hsc_env (GHC.resumeApStack r) (GHC.resumeSpan r) (GHC.resumeBreakpointId r)
        mapM tyThingToVarInfo =<< GHC.getBindings

      ModuleVariables -> Right <$> do
        things <- filter (sameMod r) <$> hugGlobalVars hsc_env r
        mapM (\(n, tt) -> do
          name <- display n
          vi <- tyThingToVarInfo tt
          return vi{varName = name}) things

      GlobalVariables -> Right <$> do
        things <- filter (not . sameMod r) <$> hugGlobalVars hsc_env r
        mapM (\(n, tt) -> do
          name <- display n
          vi <- tyThingToVarInfo tt
          return vi{varName = name}) things

      NoVariables -> Right <$> do
        return []
  where
    sameMod r (nameModule -> modl, _) = (ibi_tick_mod <$> GHC.resumeBreakpointId r) == Just modl
    hugGlobalVars hsc_env r = catMaybes <$> mapM (select hsc_env) (globalElts r)
    globalElts = globalRdrEnvElts . igre_env . snd . GHC.resumeBindings
    select hsc_env (greName -> n) = liftIO $ do
      let modl = nameModule n
      hmi <- HUG.lookupHugByModule modl (hsc_HUG hsc_env)
      case hmi of
        Nothing ->
          -- not in home units, don't show.
          pure Nothing
        Just _ -> do
          mty <- GHC.lookupType hsc_env n
          case mty of
            Nothing ->
              pure Nothing
            Just ty ->
              pure $ Just (n, ty)

--------------------------------------------------------------------------------
-- * GHC Utilities
--------------------------------------------------------------------------------

-- | Get the value and type of a given 'Name' as rendered strings in 'VarInfo'.
inspectName :: Name -> Debugger (Maybe VarInfo)
inspectName n = do
  GHC.lookupName n >>= \case
    Nothing -> do
      liftIO . putStrLn =<< display (text "Failed to lookup name: " <+> ppr n)
      pure Nothing
    Just tt -> Just <$> tyThingToVarInfo tt

-- | 'TyThing' to 'VarInfo'. The 'Bool' argument indicates whether to force the
-- value of the thing (as in @True = :force@, @False = :print@)
tyThingToVarInfo :: TyThing -> Debugger VarInfo
tyThingToVarInfo = \case
  t@(AConLike c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables <*> pure NoFields
  t@(ATyCon c)   -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables <*> pure NoFields
  t@(ACoAxiom c) -> VarInfo <$> display c <*> display t <*> display t <*> pure False <*> pure NoVariables <*> pure NoFields
  AnId i -> do
    let depth = 20 -- the depth determines how much of the structure is traversed.
                   -- using a small value like 5 here is what causes the
                   -- structure to be improperly rendered inline with many underscores.
                   -- Note: GHCi uses depth=100
                   -- TODO: Investigate why this isn't fast enough to use 100.
    term <- GHC.obtainTermFromId depth False{-don't force-} i
    termToVarInfo (GHC.idName i) term

-- | Construct a 'VarInfo' from the given 'Name' of the variable and the 'Term' it binds
termToVarInfo :: Name -> Term -> Debugger VarInfo
termToVarInfo top_name top_term = do

  -- Make a VarInfo for the top term.
  top_vi <- go top_name top_term

  sub_vis <- case top_term of
    -- Make 'VarInfo's for the first layer of subTerms only.
    Term{dc=Right dc, subTerms} -> do
      case dataConFieldLabels dc of
        -- Not a record type,
        -- Use indexed fields
        [] -> do
          let names = zipWith (\ix _ -> mkIndexVar ix) [1..] (dataConOrigArgTys dc)
          IndexedFields <$> mapM (uncurry go) (zipEqual names subTerms)
        -- Is a record type,
        -- Use field labels
        dataConFields -> do
          let names = map flSelector dataConFields
          LabeledFields <$> mapM (uncurry go) (zipEqual names subTerms)
    NewtypeWrap{dc=Right dc, wrapped_term} -> do
      case dataConFieldLabels dc of
        [] -> do
          let name = mkIndexVar 1
          wvi <- go name wrapped_term
          return (IndexedFields [wvi])
        [fld] -> do
          let name = flSelector fld
          wvi <- go name wrapped_term
          return (LabeledFields [wvi])
        _ -> error "unexpected number of Newtype fields: larger than 1"
    _ -> return NoFields

  return top_vi{varFields = sub_vis}

  where
    -- Make a VarInfo for a term, but don't recurse into the fields and return
    -- @NoFields@ for 'varFields'.
    --
    -- We do this because we don't want to recursively return all sub-fields --
    -- only the first layer of fields for the top term.
    go n term = do
      let varFields = NoFields
      let isThunk
            -- to have more information we could match further on @Heap.ClosureType@
           | Suspension{} <- term = True
           | otherwise = False
      let getSubterms t = case t of
           Prim{}                    -> []
           Term{subTerms}            -> subTerms
           NewtypeWrap{wrapped_term} -> getSubterms wrapped_term
           _                         -> []
      varName <- display n
      varType <- display (GHCI.termType term)
      varValue <- display =<< GHCD.showTerm term

      -- The VarReference allows user to expand variable structure and inspect its value.
      -- Here, we do not want to allow expanding a term that is fully evaluated.
      -- We only want to return @SpecificVariable@ (which allows expansion) for
      -- values with sub-fields or thunks.
      varRef <- do
        if GHCI.isFullyEvaluatedTerm term
           -- Even if it is already evaluated, we do want to display a
           -- structure as long if it is not a "boring type" (one that does not
           -- provide useful information from being expanded)
           -- (e.g. consider how awkward it is to expand Char# 10 and I# 20)
           && (isBoringTy (GHCI.termType term) || not (hasDirectSubTerms term))
         then
            return NoVariables
         else do
            ir <- freshInt
            insertVarReference ir n term
            return (SpecificVariable ir)

      return VarInfo{..}

    isBoringTy t = isDoubleTy t || isFloatTy t || isIntTy t || isWordTy t || isStringTy t
                    || isIntegerTy t || isNaturalTy t || isCharTy t
    hasDirectSubTerms = \case
      Suspension{}   -> False
      Prim{}         -> False
      NewtypeWrap{}  -> True
      RefWrap{}      -> True
      Term{subTerms} -> not $ null subTerms

    mkIndexVar ix = mkUnboundName (mkVarOcc ("_" ++ show @Int ix))


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
  -- TODO:
  --  [ ] Preserve bindings introduced by evaluate requests
  ioref <- asks varReferences
  liftIO $ writeIORef ioref mempty

-- | Convert a GHC's src span into an interface one
realSrcSpanToSourceSpan :: RealSrcSpan -> SourceSpan
realSrcSpanToSourceSpan ss = SourceSpan
  { file = unpackFS $ srcSpanFile ss
  , startLine = srcSpanStartLine ss
  , startCol = srcSpanStartCol ss
  , endLine = srcSpanEndLine ss
  , endCol = srcSpanEndCol ss
  }

--------------------------------------------------------------------------------
-- * General utilities
--------------------------------------------------------------------------------

-- | Display an Outputable value as a String
display :: Outputable a => a -> Debugger String
display x = do
  dflags <- getDynFlags
  return $ showSDoc dflags (ppr x)
{-# INLINE display #-}

