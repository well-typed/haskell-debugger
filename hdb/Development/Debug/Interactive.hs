{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedRecordDot, TemplateHaskell, ScopedTypeVariables #-}

module Development.Debug.Interactive where

import Data.Functor.Contravariant
import System.IO
import System.Exit
import System.Directory
import System.Console.Haskeline
-- import System.Console.Haskeline.Completion
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Options.Applicative
-- import Options.Applicative.BashCompletion

import Development.Debug.Session.Setup

import Colog.Core

import GHC.Debugger.Monad
import GHC.Debugger hiding (Command)
import Control.Monad
import Data.List (intercalate)
import GHC.Debugger.Debuggee (DebuggerLog (..), mkCliInterpreterSettings, DebuggerMessage (LogSDoc))
import GHC.Debugger.Script hiding (sendSync)
import GHC.Debugger.Backend
import Control.Concurrent
import Data.IORef
import GHC (GhcMonad, compileExpr, Ghc, setContext, InteractiveImport (IIDecl), simpleImportDecl, setGHCiMonad, TcRnExprMode (TM_Default))
import Unsafe.Coerce (unsafeCoerce)

import GHC.Debugger.Session (interactiveGhcDebuggerUnitId, addInteractiveGhcDebuggerUnit', modifyInteractiveDebuggerDynFlags)
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Plugins as GHC hiding (exprType)
import qualified GHC.Debugger.Interface.Messages as D
import qualified GHC
import System.Environment
import qualified GHC.Runtime.Context as GHCi
import GHCi.RemoteTypes (HValue(..))
import qualified GHCi.RemoteTypes as GHC
import qualified GHC.Linker.Loader as Loader
import qualified GHC.Runtime.Interpreter as GHC
import qualified Data.Text as T
import qualified Control.Monad.Catch.Pure as MC
import GHC.Debugger.Session.Builtin (debuggerScriptUnitId)
import qualified Colog.Core as Logger

-- hdsUnitId :: GHC.UnitId
-- hdsUnitId = GHC.toUnitId . GHC.stringToUnit $ $(maybe (fail "no package") TH.lift (namePackage 'Script))



-------------------------------------------
-- Script Runtime
-------------------------------------------
-- TODO: move somewhere else.

-- | Bind a host value under 'name'. 'tyStr' is the value's type written
-- as Haskell source, resolved in the current interactive context.
bindHostValue :: String -> String -> a -> Ghc ()
bindHostValue name tyStr val = do
  -- Get the GHC 'Type' by type-checking a dummy expression
  ty <- GHC.exprType GHC.TM_Default ("undefined :: " ++ tyStr)

  us <- liftIO (GHC.mkSplitUniqSupply 'H')   -- tag char is arbitrary
  let n  = GHC.mkInternalName (GHC.uniqFromSupply us) (GHC.mkVarOcc name) GHC.noSrcSpan
      i  = GHC.mkVanillaGlobal n ty

  hsc_env <- GHC.getSession
  let interp = GHC.hscInterp hsc_env

  -- Wrap the value (internal interpreter: this is just a StablePtr)
  fhv <- liftIO $ GHC.mkRemoteRef (HValue (unsafeCoerce val))
                  >>= GHC.mkFinalizedHValue interp

  -- Make the name resolvable at runtime...
  liftIO $ do
    Loader.initLoaderState interp hsc_env
    Loader.extendLoadedEnv interp [(n, fhv)]
  -- ...and visible to the renamer/typechecker
  GHC.modifySession $ \env ->
    env { GHC.hsc_IC = GHCi.extendInteractiveContextWithIds (GHC.hsc_IC env) [i] }



data ScriptLog
  = ScriptMsg ScriptMsg  -- outputs/errors from scripts
  | DbgLog DebuggerLog   -- setting up ghc session

newtype ScriptCommand = ScriptCommand String

initScriptRuntime :: LogAction IO InteractiveLog -> DebugRunnerConf -> RunDebuggerSettings -> DebugExecutionArgs -> IO ScriptRuntime
initScriptRuntime logger conf runConf dbgExecArgs = do
  let hieBiosLogger = contramap ISessionSetupLog logger
  hieDebugRunner hieBiosLogger conf >>= \case
   Left e               -> do
     hPutStrLn stderr e
     exitWith (ExitFailure 33)
   Right (ghcInvocation, debugRunner) -> do
     let debugRec = contramap IDebuggerLog logger
     (backend, backendAct) <- debuggerThreadBackend debugRec debugRunner runConf
     backendThread <- forkIO backendAct
     (sendScriptCommand, scriptEvalAct) <-
       scriptEvalThread (contramap (IScriptLog . ScriptMsg) logger) backend ghcInvocation dbgExecArgs
     scriptThread <- forkIO scriptEvalAct
     pure $ ScriptRuntime{..}
  where
    scriptEvalThread :: LogAction IO ScriptMsg -> DbgBackend
                     -> GhcInvocation -> DebugExecutionArgs
                     -> IO (ScriptCommand -> IO (), IO ())
    scriptEvalThread (LogAction _output) dbgBackend gi debugExecutionArgs = do
      cmds <- newChan
      let sendCmd = writeChan cmds
      pure (sendCmd, thread $ readChan cmds) where
      -- TODO: this whole thread should run in a separate process and communicate via pipe.
      thread getCmd = do
        GotGhcSessionInfo ghcs <- sendSync dbgBackend GetGhcSessionInfo
        currentStop <- newIORef NotStopped
        let
          -- TODO: haskell-debugger-script is not the one we are linked against, so compiled code and runtime code should interact via a ghc-boot-libraries-only API.
          scriptEnv = SEE
            { dbgBackend
            , _output
            , currentStop
            , debugExecutionArgs
            }
        setupScriptEvalSession gi ghcs $ do
          withGhcLoggerToDebug (contramap (IScriptLog . DbgLog) logger) $ do

            GHC.setContext [ IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName s
                           | s <- ["Prelude", "GHC.Debugger.Script", "GHC.GHCi"] ]

            bindHostValue "_script_eval_env" "ScriptEvalEnv" scriptEnv

            _ <- GHC.runDecls "instance GHC.GHCi.GHCiSandboxIO Script where ghciStepIO = runScript _script_eval_env"
            GHC.setGHCiMonad "Script"

            [replPrintName] <- GHC.runDecls "replPrintDef x = runScript _script_eval_env (replPrint x)"

            GHC.modifySession $ \hsc_env ->
              hsc_env {GHC.hsc_IC = GHCi.setInteractivePrintName (GHC.hsc_IC hsc_env) replPrintName}

          forever $ liftIO getCmd >>= \(ScriptCommand cmd) -> do
            MC.try @_ @GHC.SourceError (GHC.execStmt cmd GHC.execOptions) >>= \case
              Left e -> liftIO $ _output $ ErrorMessage (T.pack $ show e)
              Right _ -> pure ()

    getDepsAndDBs GhcSessionInfo{..} = do

      hDB_SCRIPT_PACKAGEDBS <- liftIO $ lookupEnv "HDB_SCRIPT_PACKAGEDBS"
      let dbs = ghcSessionPackageDBs ++
            case hDB_SCRIPT_PACKAGEDBS of
              Nothing -> []
              Just s  -> map (GHC.PackageDB . GHC.PkgDbPath) (read s)
      -- TODO debug log dbs

      base <- GHC.baseUnitId <$> GHC.getDynFlags
      -- Just deps' <- pure $ mapM (GHC.lookupPackageName us . GHC.PackageName) ["ghc-heap","ghci"]
      -- TODO: discover or compile in memory
      let deps = [base]

      pure (deps,dbs)

    setupScriptEvalSession :: GhcInvocation -> GhcSessionInfo -> Ghc () -> IO ()
    setupScriptEvalSession GhcInvocation{..} debuggeeSession m = runGhc (Just gi_libdir) $ do
                                              -- internal, normal stdin.
      cliIS <- liftIO $ mkCliInterpreterSettings True Nothing
      let runConf' = runConf {interpreterSettings = cliIS }
          dbgL = contramap (IScriptLog . DbgLog) logger
      -- TODO: disable writing .hi and .gbc files or set a dump-dir.
      runner dbgL runConf' (unAbs $ projectRoot dbgExecArgs) gi_args id $ do
        (deps,dbs) <- getDepsAndDBs debuggeeSession

        GHC.modifySessionM $ liftIO . addInteractiveGhcDebuggerUnit' dbs deps
        GHC.modifySession $ GHC.hscSetActiveUnitId interactiveGhcDebuggerUnitId
        dflags <- GHC.getDynFlags
        loadScript dbgL (GHC.targetWays_ dflags)
        GHC.modifySessionM $ liftIO . addInteractiveGhcDebuggerUnit' dbs (debuggerScriptUnitId:deps)

        modifyInteractiveDebuggerDynFlags $ \ df ->
          df
          `GHC.gopt_unset` GHC.Opt_NoIt

        m

withGhcLoggerToDebug :: GhcMonad m => LogAction IO DebuggerLog -> m () -> m ()
withGhcLoggerToDebug l m = do
  dflags <- GHC.getSessionDynFlags
  GHC.modifyLogger $
    -- Emit it all as Debug-level debugger logs
    GHC.pushLogHook $ const $ \_ _ _ sdoc ->
      l <& DebuggerLog Logger.Debug (LogSDoc dflags sdoc)
  m
  -- Restore logger
  GHC.modifyLogger $
    GHC.popLogHook

sendSync :: DbgBackend -> D.Command -> IO Response
sendSync DbgBackend{..} cmd = do
  syncRequests cmd
  syncResponses

data ScriptRuntime = ScriptRuntime
  { sendScriptCommand :: ScriptCommand -> IO ()
  , backendThread :: ThreadId
  , scriptThread :: ThreadId
    -- ^ output is sent back via the logger.
  }

compileScript :: GhcMonad m => ScriptCommand -> m (Script ())
compileScript (ScriptCommand cmd) = do
  fmap unsafeCoerce . compileExpr $ concat ["(",cmd,")"] ++ " :: Script ()"

-------------------------------------------

data RunContext = RunContext
  { runLastCommand :: Maybe ScriptCommand
  }

-- | Interactive debugging monad
type InteractiveDM a = InputT (RWST ScriptRuntime () RunContext IO) a

data InteractiveLog
  = IDebuggerLog DebuggerLog
  | ISessionSetupLog (WithSeverity SessionSetupLog)
  | IScriptLog ScriptLog

-- | Run it
runIDM :: LogAction IO InteractiveLog
       -> String   -- ^ entryPoint
       -> FilePath -- ^ entryFile
       -> [String] -- ^ entryArgs
       -> [String] -- ^ extraGhcArgs
       -> Maybe FilePath
       -> RunDebuggerSettings
       -> InteractiveDM a
       -> IO a
runIDM logger runEntryPoint runEntryFile runArgs extraGhcArgs cradleFile runConf act = do
  projectRoot <- mkAbsolute <$> getCurrentDirectory

  let entryFile = projectRoot /> runEntryFile

  entryFileExists <- doesFileExist (unAbs entryFile)
  when (not entryFileExists) $ do
    exitWithMsg $ "Entry file \"" ++ (unAbs entryFile) ++ "\" does not exist or is a directory."

  let dbgRunConf = DebugRunnerConf (unAbs projectRoot) (unAbs entryFile) extraGhcArgs cradleFile
  let entryPoint
        | runEntryPoint == "main" = MainEntry Nothing
        | otherwise            = FunctionEntry runEntryPoint
  let dbgExecArgs = DebugExecutionArgs {..}

  scriptRuntime <- initScriptRuntime logger dbgRunConf runConf dbgExecArgs
  fmap fst $
          evalRWST (runInputT (setComplete noCompletion defaultSettings) act)
                   scriptRuntime
                   (RunContext { runLastCommand = Nothing } )
  where
    exitWithMsg txt = do
      hPutStrLn stderr txt
      exitWith (ExitFailure 33)


  --   completeF = completeWordWithPrev Nothing filenameWordBreakChars $
  --     \(reverse -> previous) word -> do
  --       let comp_words = words previous ++ [word]
  --           comp_cword = length comp_words
  --       case execParserPure parserPrefs cmdParserInfo
  --           ("--bash-completion-index":show comp_cword:
  --             concat (zipWith (\fl a -> [fl, show a]) (repeat "--bash-completion-word") comp_words)) of
  --         CompletionInvoked CompletionResult{execCompletion} ->
  --           map simpleCompletion . words <$> liftIO (execCompletion "")
  --         _ -> return []

-- | Run the interactive command-line debugger
debugInteractive :: InteractiveDM ()
debugInteractive = withInterrupt loop
  where
    loop = handleInterrupt loop $ do
      minput <- getInputLine "(hdb) "
      case minput of
        Nothing -> outputStrLn "Exiting..." >> liftIO (exitWith ExitSuccess)
        Just "" -> do
          lift (gets runLastCommand) >>= \case
            Nothing -> return ()
            Just cmd -> do
              interpretCmd cmd -- repeat last command
        Just input -> do
          mcmd <- parseCmd input
          case mcmd of
            Nothing -> return ()
            Just Exit -> outputStrLn "Exiting..." >> liftIO (exitWith ExitSuccess)
            Just (Do cmd) -> do
              -- TODO: guard on repeatable commands somehow, e.g. exclude run
              lift $ modify (\ o -> o { runLastCommand = Just cmd })
              interpretCmd cmd
      loop

interpretCmd :: ScriptCommand -> InteractiveDM ()
interpretCmd scmd = do
  ScriptRuntime{..} <- lift ask
  liftIO $ sendScriptCommand scmd

{-

-}

-- showExceptionDetails :: RemoteThreadId -> InteractiveDM ()
-- showExceptionDetails tid = do
--   infoResp <- lift . lift $ execute (GetExceptionInfo tid)
--   case infoResp of
--     GotExceptionInfo exc_info -> outputStrLn $ renderExceptionInfo exc_info
--     _ -> pure ()
--   stackResp <- lift . lift $ execute (GetStacktrace tid)
--   case stackResp of
--     GotStacktrace (frame:_) ->
--       outputStrLn $
--         "Exception location: " ++ renderSourceSpan (frame.sourceSpan)
--     _ -> outputStrLn "Exception location: <unknown>"

--------------------------------------------------------------------------------
-- Printing
--------------------------------------------------------------------------------

-- printResponse :: Response -> InteractiveDM ()
-- printResponse = \case
--   DidEval er -> outputStrLn (showEvalResult er)
--       -- don't remember thread context for eval requests
--       --
--       -- FIXME: we should track which threads we've started and which have been
--       -- stopped per-thread rather than with a global one, then we could do
--       -- this more uniformly.
--   DidSetBreakpoint bf       -> outputStrLn $ show bf
--   DidRemoveBreakpoint bf    -> outputStrLn $ show bf
--   DidGetBreakpoints mb_span -> outputStrLn $ show mb_span
--   DidClearBreakpoints -> outputStrLn "Cleared all breakpoints."
--   DidResume er -> outputEvalResult er
--   DidExec er -> outputEvalResult er
--   DidTerminate b -> outputStrLn $ if b then "Terminated." else "Could not terminate."
--   GotThreads threads -> outputStrLn $ show threads
--   GotStacktrace stackframes -> outputStrLn $ show stackframes
--   GotScopes scopeinfos -> outputStrLn $ show scopeinfos
--   GotVariables vis -> outputVariables vis
--   GotExceptionInfo exc_info -> outputStrLn $ renderExceptionInfo exc_info
--   Aborted err_str -> outputStrLn ("Aborted: " ++ err_str)
--   NonFatalError err_str -> outputStrLn ("Encountered error: " ++ err_str)
--   Initialised -> pure ()
--   where
--     outputEvalResult er = do
--       case er of
--         EvalStopped{breakThread} -> do
--           cmd <- lift $ gets runLastCommand
--           if isStepCmd cmd then do
--              -- Always print the stopped scope if stopped?
--              -- FIXME: Figure out the CLI interface.
--              out <- lift . lift $ execute (GetScopes breakThread 0)
--              printResponse out
--           else do
--              outputStrLn (showEvalResult er)
--         _ -> outputStrLn (showEvalResult er)
--       maybeShowException er
--       rememberThreadContext er

--     maybeShowException EvalStopped{breakId = Nothing, breakThread=tid} =
--       showExceptionDetails tid
--     maybeShowException _ = pure ()

--     rememberThreadContext er =
--       case er of
--         EvalCompleted{} -> lift $ modify' (\ ctx -> ctx { runCurrentThread = Nothing } )
--         EvalException{} -> pure () -- TODO: exceptions are still not signaling per-thread
--         EvalStopped{breakThread} -> lift $ modify' (\ ctx -> ctx { runCurrentThread = Just breakThread } )
--         EvalAbortedWith{} -> lift $ modify' (\ ctx -> ctx { runCurrentThread = Nothing } )

--     outputVariables (ForcedVariable var) = outputVariables (VariableFields [var])
--     outputVariables (VariableFields vars) = do
--        ctx <- lift get
--        case runCurrentThread ctx of
--          Just threadId ->
--           mapM_ (outputVarWithFields  threadId 0) vars
--          Nothing -> error "no thread id"

--     outputVarWithFields threadId frameIx var = do
--       outputStrLn (showVarInfo var)
--       fields <- fetchFields threadId frameIx var
--       mapM_ (outputStrLn . ("  " ++) . showVarInfo) fields

--     fetchFields _ _ VarInfo{varRef = NoVariables} = pure []
--     fetchFields threadId frameIx VarInfo{varRef = ref@(SpecificVariable _), ..} = do
--       resp <- lift . lift $ execute (GetVariables threadId frameIx ref)
--       case resp of
--         GotVariables res -> pure (variableResultToList res)
--         Aborted err -> outputStrLn ("Failed to fetch fields for " ++ varName ++ ": " ++ err) >> pure []
--         _ -> outputStrLn ("Unexpected response when fetching fields for " ++ varName) >> pure []
--     fetchFields _ _ _ = pure []

--     isStepCmd (Just (DoResume _ s _))
--       | ResumeNoStep <- s = False
--       | otherwise         = True
--     isStepCmd _           = False

showEvalResult :: EvalResult -> String
showEvalResult (EvalCompleted{..}) = resultVal
showEvalResult (EvalException{..}) = resultVal
showEvalResult (EvalStopped{}) = "Stopped at breakpoint"
showEvalResult (EvalAbortedWith err) = "Aborted: " ++ err

showVarInfoResult :: VariableResult -> String
showVarInfoResult (ForcedVariable vi) = showVarInfo vi
showVarInfoResult (VariableFields vis) = unlines $ map showVarInfo vis

showVarInfo :: VarInfo -> String
showVarInfo VarInfo{..} = unwords [varName, ":", varType, "=", varValue]

renderSourceSpan :: SourceSpan -> String
renderSourceSpan SourceSpan{..} =
  unAbs file ++ ":" ++ show startLine ++ ":" ++ show startCol

renderExceptionInfo :: ExceptionInfo -> String
renderExceptionInfo = unlines . go 0
  where
    go depth exInfo =
      let indent = replicate (depth * 2) ' '
          typeLine = indent ++ "Exception: " ++ exceptionInfoTypeName exInfo
          messageLine = indent ++ "Message: " ++ exceptionInfoMessage exInfo
          ctxLine = case exceptionInfoContext exInfo of
            Nothing -> indent ++ "Call stack: <unavailable>"
            Just ctx -> indent ++ "Call stack:\n" ++ indentMultiline (depth + 1) ctx
          innerLines = case exceptionInfoInner exInfo of
            [] -> []
            xs -> (indent ++ "Inner exceptions:") : concatMap (go (depth + 1)) xs
      in typeLine : messageLine : ctxLine : innerLines

    indentMultiline depth txt =
      let pref = replicate (depth * 2) ' '
      in intercalate "\n" (map (pref ++) (lines txt))

--------------------------------------------------------------------------------
-- Command parser
--------------------------------------------------------------------------------

breakpointParser :: AbsFilePath -> Parser Breakpoint
breakpointParser root =
  ( ModuleBreak
  <$> argument ((root />) <$> str)
      ( metavar "PATH" -- todo: accept module breaks using module name
     <> help "Path to module to break at" )
  <*> argument auto
      ( metavar "LINE_NUM"
     <> help "The line number to break at" )
  <*> optional (argument auto
      ( metavar "COLUMN_NUM"
     <> help "The column number to break at" ))
  )
  <|>
  ( FunctionBreak
    <$> option str
      ( long "name"
     <> short 'n'
     <> metavar "FUNCTION_NAME"
     <> help "Set a breakpoint using the function name" )
  )
  <|>
  ( flag' OnExceptionsBreak ( long "exceptions" )
  )
  <|>
  ( flag' OnUncaughtExceptionsBreak ( long "error" )
  )

conditionalBreakParser :: Parser (Maybe String)
conditionalBreakParser =
  optional (option str
    ( long "condition"
   <> metavar "CONDITION"
   <> help "Only stop when CONDITION evaluates to True" ))

hitCountBreakParser :: Parser (Maybe Int)
hitCountBreakParser =
  optional (option auto
    ( long "hit"
   <> metavar "N:INT"
   <> help "Ignore first N:INT times this breakpoint is hit" ))

data OrExit a = Do a
              | Exit
  deriving Functor

-- | TODO: handle this as part of issue #144
logMessageParser :: Parser (Maybe String)
logMessageParser = pure Nothing


-- | Parse command line arguments
parseCmd :: String -> InteractiveDM (Maybe (OrExit ScriptCommand))
parseCmd input = do
  _opts <- lift ask
  _ctx <- lift get
  let
    res = case input of
      "exit" -> Success Exit
      s -> Success (Do $ ScriptCommand s)
   in case res of
    Success cmd ->
      return (Just cmd)
    Failure bad ->
      let (msg, _exit) = renderFailure bad "(hdb)"
       in outputStrLn msg >> pure Nothing
    _ -> outputStrLn "Unsupported command parsing mode" >> pure Nothing

parserPrefs :: ParserPrefs
parserPrefs = prefs (disambiguate <> showHelpOnError <> showHelpOnEmpty)
