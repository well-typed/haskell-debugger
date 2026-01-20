{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedRecordDot #-}
module Development.Debug.Interactive where

import Data.Functor.Contravariant
import System.IO
import System.Exit
import System.Directory
import System.Console.Haskeline
-- import System.Console.Haskeline.Completion
import System.FilePath
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Options.Applicative
-- import Options.Applicative.BashCompletion

import Development.Debug.Session.Setup

import Colog.Core
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Monad
import GHC.Debugger
import Control.Monad
import Data.List (intercalate)
import qualified Data.Maybe as Maybe

data RunOptions = RunOptions
  { runEntryFile :: FilePath
  , runEntryPoint :: String
  , runEntryArgs :: [String]
  }

data RunContext = RunContext
  { runCurrentThread :: Maybe RemoteThreadId
  , runLastCommand :: Maybe Command
  }

-- | Interactive debugging monad
type InteractiveDM a = InputT (RWST RunOptions () RunContext Debugger) a

data InteractiveLog
  = IDebuggerLog DebuggerLog
  | ISessionSetupLog (WithSeverity SessionSetupLog)

-- | Run it
runIDM :: LogAction IO InteractiveLog
       -> String   -- ^ entryPoint
       -> FilePath -- ^ entryFile
       -> [String] -- ^ entryArgs
       -> [String] -- ^ extraGhcArgs
       -> RunDebuggerSettings
       -> InteractiveDM a
       -> IO a
runIDM logger entryPoint entryFile entryArgs extraGhcArgs runConf act = do
  projectRoot <- getCurrentDirectory

  let hieBiosLogger = contramap ISessionSetupLog logger
  runExceptT (hieBiosSetup hieBiosLogger projectRoot entryFile) >>= \case
    Left e               -> exitWithMsg e
    Right (Left e)       -> exitWithMsg e
    Right (Right flags)
      | HieBiosFlags{..} <- flags
                         -> do

      let absEntryFile = normalise $ projectRoot </> entryFile
      let debugRec = contramap IDebuggerLog logger

      runDebugger debugRec rootDir componentDir libdir units ghcInvocation extraGhcArgs absEntryFile runConf $
        fmap fst $
          evalRWST (runInputT (setComplete noCompletion defaultSettings) act)
                   (RunOptions { runEntryFile = entryFile, runEntryPoint = entryPoint, runEntryArgs = entryArgs })
                   (RunContext { runLastCommand = Nothing, runCurrentThread = Nothing } )
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
              out <- lift . lift $ execute cmd -- repeat last command
              printResponse out
        Just input -> do
          mcmd <- parseCmd input
          lift $ modify (\ o -> o { runLastCommand = mcmd })
          case mcmd of
            Nothing -> return ()
            Just cmd -> do
              out <- lift . lift $ execute cmd
              printResponse out
      loop

showExceptionDetails :: RemoteThreadId -> InteractiveDM ()
showExceptionDetails tid = do
  infoResp <- lift . lift $ execute (GetExceptionInfo tid)
  case infoResp of
    GotExceptionInfo exc_info -> outputStrLn $ renderExceptionInfo exc_info
    _ -> pure ()
  stackResp <- lift . lift $ execute (GetStacktrace tid)
  case stackResp of
    GotStacktrace (frame:_) ->
      outputStrLn $
        "Exception location: " ++ renderSourceSpan (frame.sourceSpan)
    _ -> outputStrLn "Exception location: <unknown>"

--------------------------------------------------------------------------------
-- Printing
--------------------------------------------------------------------------------

printResponse :: Response -> InteractiveDM ()
printResponse = \case
  DidEval er -> outputEvalResult er
  DidSetBreakpoint bf       -> outputStrLn $ show bf
  DidRemoveBreakpoint bf    -> outputStrLn $ show bf
  DidGetBreakpoints mb_span -> outputStrLn $ show mb_span
  DidClearBreakpoints -> outputStrLn "Cleared all breakpoints."
  DidContinue er -> outputEvalResult er
  DidStep er -> printEvalResult er
  DidExec er -> outputEvalResult er
  GotThreads threads -> outputStrLn $ show threads
  GotStacktrace stackframes -> outputStrLn $ show stackframes
  GotScopes scopeinfos -> outputStrLn $ show scopeinfos
  GotVariables vis -> outputVariables vis
  GotExceptionInfo exc_info -> outputStrLn $ renderExceptionInfo exc_info
  Aborted err_str -> outputStrLn ("Aborted: " ++ err_str)
  Initialised -> pure ()
  where
    outputEvalResult er = do
      outputStrLn (showEvalResult er)
      maybeShowException er
      rememberThreadContext er

    maybeShowException EvalStopped{breakId = Nothing, breakThread=tid} =
      showExceptionDetails tid
    maybeShowException _ = pure ()

    rememberThreadContext er =
      case er of
        EvalCompleted{} -> lift $ modify' (\ ctx -> ctx { runCurrentThread = Nothing } )
        EvalException{} -> pure () -- TODO: why does this not have a thread associated?
        EvalStopped{breakThread} -> lift $ modify' (\ ctx -> ctx { runCurrentThread = Just breakThread } )
        EvalAbortedWith{} -> lift $ modify' (\ ctx -> ctx { runCurrentThread = Nothing } )

    outputVariables (ForcedVariable var) = outputVariables (VariableFields [var])
    outputVariables (VariableFields vars) = do
       ctx <- lift get
       case runCurrentThread ctx of
         Just threadId ->
          mapM_ (outputVarWithFields  threadId 0) vars
         Nothing -> error "no thread id"

    outputVarWithFields threadId frameIx var = do
      outputStrLn (showVarInfo var)
      fields <- fetchFields threadId frameIx var
      mapM_ (outputStrLn . ("  " ++) . showVarInfo) fields

    fetchFields _ _ VarInfo{varRef = NoVariables} = pure []
    fetchFields threadId frameIx VarInfo{varRef = ref@(SpecificVariable _), ..} = do
      resp <- lift . lift $ execute (GetVariables threadId frameIx ref)
      case resp of
        GotVariables res -> pure (variableResultToList res)
        Aborted err -> outputStrLn ("Failed to fetch fields for " ++ varName ++ ": " ++ err) >> pure []
        _ -> outputStrLn ("Unexpected response when fetching fields for " ++ varName) >> pure []
    fetchFields _ _ _ = pure []

printEvalResult :: EvalResult -> InteractiveDM ()
printEvalResult EvalStopped{..} = do
  out <- lift . lift $ execute (GetScopes breakThread 0)
  printResponse out
  when (breakId == Nothing) $
    showExceptionDetails breakThread
printEvalResult er = outputStrLn $ showEvalResult er

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
  file ++ ":" ++ show startLine ++ ":" ++ show startCol

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

breakpointParser :: Parser Breakpoint
breakpointParser =
  ( ModuleBreak
  <$> argument str
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

runParser :: RunOptions -> Parser Command
runParser opts =
  -- --entry <name> with some args
  -- (DebugExecution <$> parseEntry <*> parseSomeArgs)
  -- --entry <name> without any args
  -- <|> (DebugExecution <$> parseEntry <*> pure [])
  -- just some args
  (DebugExecution (mkEntry (runEntryPoint opts)) (runEntryFile opts) <$> parseSomeArgs)
  -- just "run"
  <|> (pure $ DebugExecution (mkEntry (runEntryPoint opts)) (runEntryFile opts) (runEntryArgs opts))
  where
    _parseEntry =
      fmap mkEntry $
      option str
        ( long "entry"
        <> short 'e'
        <> metavar "FUNCTION_NAME"
        <> help "Run with this entry point"
        )
    parseSomeArgs =
      some ( argument str
        ( metavar "ARGS" <> help "Arguments to pass to the entry point. If empty, the arguments given at the debugger invocation are used." ) )
    mkEntry entry
      | entry == "main" = MainEntry Nothing
      | otherwise = FunctionEntry (runEntryPoint opts)

-- | Combined parser for 'Command'
cmdParser :: RunOptions -> RunContext -> Parser Command
cmdParser opts ctx = hsubparser
   (
    Options.Applicative.command "delete"
    ( info (DelBreakpoint <$> breakpointParser)
      ( progDesc "Delete a breakpoint" ) )
  <>
    Options.Applicative.command "run"
    ( info (runParser opts)
      ( progDesc "Run the debuggee" ) )
  <>
    Options.Applicative.command "next"
    ( info (pure DoStepLocal)
      ( progDesc "Step over to the next line" ) )
  <>
    Options.Applicative.command "step"
    ( info (pure DoSingleStep)
      ( progDesc "Step-in to the next immediate location" ) )
  <>
    Options.Applicative.command "finish"
    ( info (pure DoStepOut)
      ( progDesc "Step-out of the current function into the caller/its continuation" ) )
  <>
    Options.Applicative.command "continue"
    ( info (pure DoContinue)
      ( progDesc "Continue executing from the current breakpoint" ) )
  <>
    Options.Applicative.command "print"
    ( info (DoEval . unwords <$> many (argument str ( metavar "EXPRESSION"
     <> help "Expression to evaluate in the current context" )))
      ( progDesc "Evaluate an expression in the current context" ) )
  <>
    Options.Applicative.command "exit"
    ( info (pure TerminateProcess)
      ( progDesc "Terminate and exit the debugger session" ) )
  <>
    Options.Applicative.command "threads"
    ( info (pure GetThreads)
      ( progDesc "Print all user threads" ) )
  <>
    Options.Applicative.command "backtrace"
    ( info (stackTraceParser ctx <**> helper)
      ( progDesc "Print stack trace" ) )
  <>
    Options.Applicative.command "variables"
    ( info (variablesParser ctx <**> helper)
      ( progDesc "Print local variables" ) )
  <> Options.Applicative.command "break"
    ( info (SetBreakpoint <$> breakpointParser <*> hitCountBreakParser <*> conditionalBreakParser)
      ( progDesc "Set a breakpoint" ) )
  )

stackTraceParser :: RunContext -> Parser Command
stackTraceParser ctx =
  GetStacktrace <$> threadIdParser ctx "Print backtrace of THREAD_ID. Defaults to current thread at breakpoint."

variablesParser :: RunContext -> Parser Command
variablesParser ctx =
  GetVariables
    <$> threadIdParser ctx "Show variables of THREAD_ID. Defaults to current thread at breakpoint."
    <*> pure 0
    <*> pure LocalVariables

threadIdParser :: RunContext -> String -> Parser RemoteThreadId
threadIdParser ctx helpMsg =
     RemoteThreadId <$> argument auto (metavar "THREAD_ID" <> help helpMsg)
 <|> Maybe.maybe (empty <**> abortOption (ErrorMsg "Not stopped at a Breakpoint") mempty) pure (runCurrentThread ctx)

-- | Main parser info
cmdParserInfo :: RunOptions -> RunContext -> ParserInfo Command
cmdParserInfo opts ctx = info (cmdParser opts ctx)
  ( fullDesc )

-- | Parse command line arguments
parseCmd :: String -> InteractiveDM (Maybe Command)
parseCmd input = do
  opts <- lift ask
  ctx <- lift get
  let
    res = execParserPure
     parserPrefs
     (cmdParserInfo opts ctx)
     (words input)
   in case res of
    Success x ->
      return (Just x)
    Failure bad ->
      let (msg, _exit) = renderFailure bad "(hdb)"
       in outputStrLn msg >> pure Nothing
    _ -> outputStrLn "Unsupported command parsing mode" >> pure Nothing

parserPrefs :: ParserPrefs
parserPrefs = prefs (disambiguate <> showHelpOnError <> showHelpOnEmpty)
