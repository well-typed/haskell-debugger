{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards #-}
module Development.Debug.Interactive where

import System.IO
import System.Exit
import System.Directory
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import System.FilePath
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Options.Applicative
import Options.Applicative.BashCompletion

import Development.Debug.Session.Setup

import GHC.Debugger.Logger
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Monad
import GHC.Debugger

-- | Interactive debugging monad
type InteractiveDM a = InputT (RWST (FilePath{-entry file-},String{-entry point-}, [String]{-run args-}) ()
                                (Maybe Command{-last cmd-}) Debugger) a

data InteractiveLog
  = DebuggerLog DebuggerLog
  | DebuggerMonadLog DebuggerMonadLog
  | FlagsLog FlagsLog

instance Pretty InteractiveLog where
  pretty = \ case
    DebuggerLog msg -> pretty msg
    FlagsLog msg -> pretty msg
    DebuggerMonadLog msg -> pretty msg

-- | Run it
runIDM :: Recorder (WithSeverity InteractiveLog)
       -> String   -- ^ entryPoint
       -> FilePath -- ^ entryFile
       -> [String] -- ^ entryArgs
       -> [String] -- ^ extraGhcArgs
       -> InteractiveDM a
       -> IO a
runIDM logger entryPoint entryFile entryArgs extraGhcArgs act = do
  projectRoot <- getCurrentDirectory

  let hieBiosLogger = cmapWithSev FlagsLog logger
  runExceptT (hieBiosSetup hieBiosLogger projectRoot entryFile) >>= \case
    Left e               -> exitWithMsg e
    Right (Left e)       -> exitWithMsg e
    Right (Right flags)
      | HieBiosFlags{..} <- flags
                         -> do

      let defaultRunConf = RunDebuggerSettings
            { supportsANSIStyling = True -- todo: check!!
            , supportsANSIHyperlinks = False
            }

      let absEntryFile = normalise $ projectRoot </> entryFile
      let debugRec = cmapWithSev DebuggerMonadLog logger

      runDebugger debugRec stdout rootDir componentDir libdir units ghcInvocation extraGhcArgs absEntryFile defaultRunConf $
        fmap fst $
          evalRWST (runInputT (setComplete noCompletion defaultSettings) act)
                   (entryFile, entryPoint, entryArgs) Nothing
  where
    exitWithMsg txt = do
      putStrLn txt
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
debugInteractive :: Recorder (WithSeverity InteractiveLog) -> InteractiveDM ()
debugInteractive recorder = withInterrupt loop
  where
    debugRec = cmapWithSev DebuggerLog recorder
    loop = handleInterrupt loop $ do
      minput <- getInputLine "(hdb) "
      case minput of
        Nothing -> outputStrLn "Exiting..." >> liftIO (exitWith ExitSuccess)
        Just "" -> do
          lift get >>= \case
            Nothing -> return ()
            Just (cmd :: Command) -> do
              out <- lift . lift $ execute debugRec cmd -- repeat last command
              printResponse debugRec out
        Just input -> do
          mcmd <- parseCmd input
          lift $ put mcmd
          case mcmd of
            Nothing -> return ()
            Just cmd -> do
              out <- lift . lift $ execute debugRec cmd
              printResponse debugRec out
      loop

--------------------------------------------------------------------------------
-- Printing
--------------------------------------------------------------------------------

printResponse :: Recorder (WithSeverity DebuggerLog) -> Response -> InteractiveDM ()
printResponse recd = \case
  DidEval er -> outputStrLn $ show er
  DidSetBreakpoint bf       -> outputStrLn $ show bf
  DidRemoveBreakpoint bf    -> outputStrLn $ show bf
  DidGetBreakpoints mb_span -> outputStrLn $ show mb_span
  DidClearBreakpoints -> outputStrLn "Cleared all breakpoints."
  DidContinue er -> outputStrLn $ show er
  DidStep er -> printEvalResult recd er
  DidExec er -> outputStrLn $ show er
  GotStacktrace stackframes -> outputStrLn $ show stackframes
  GotScopes scopeinfos -> outputStrLn $ show scopeinfos
  GotVariables vis -> outputStrLn $ show vis -- (Either VarInfo [VarInfo])
  Aborted str -> outputStrLn ("Aborted: " ++ str)
  Initialised -> pure ()

printEvalResult :: Recorder (WithSeverity DebuggerLog) -> EvalResult -> InteractiveDM ()
printEvalResult recd EvalStopped{breakId} = do
  out <- lift . lift $ execute recd GetScopes
  printResponse recd out
printEvalResult _ er = outputStrLn $ show er

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

runParser :: FilePath -> String -> [String] -> Parser Command
runParser entryFile entryPoint entryArgs =
  -- --entry <name> with some args
  -- (DebugExecution <$> parseEntry <*> parseSomeArgs)
  -- --entry <name> without any args
  -- <|> (DebugExecution <$> parseEntry <*> pure [])
  -- just some args
  (DebugExecution (mkEntry entryPoint) entryFile <$> parseSomeArgs)
  -- just "run"
  <|> (pure $ DebugExecution (mkEntry entryPoint) entryFile entryArgs)
  where
    parseEntry =
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
      | otherwise = FunctionEntry entryPoint

-- | Combined parser for 'Command'
cmdParser :: FilePath -> String -> [String] -> Parser Command
cmdParser entryFile entryPoint entryArgs = hsubparser
  ( Options.Applicative.command "break"
    ( info (SetBreakpoint <$> breakpointParser <*> hitCountBreakParser <*> conditionalBreakParser)
      ( progDesc "Set a breakpoint" ) )
  <>
    Options.Applicative.command "delete"
    ( info (DelBreakpoint <$> breakpointParser)
      ( progDesc "Delete a breakpoint" ) )
  <>
    Options.Applicative.command "run"
    ( info (runParser entryFile entryPoint entryArgs)
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
  )

-- | Main parser info
cmdParserInfo :: FilePath -> String -> [String] -> ParserInfo Command
cmdParserInfo entryFile entryPoint entryArgs = info (cmdParser entryFile entryPoint entryArgs)
  ( fullDesc )

-- | Parse command line arguments
parseCmd :: String -> InteractiveDM (Maybe Command)
parseCmd input = do
  (entryFile, entryPoint, entryArgs) <- lift ask
  let
    res = execParserPure
     parserPrefs
     (cmdParserInfo entryFile entryPoint entryArgs)
     (words input)
   in case res of
    Success x ->
      return (Just x)
    Failure bad ->
      let (msg, _exit) = renderFailure bad "(hdb)"
       in outputStrLn msg >> pure Nothing
    _ -> outputStrLn "Unsupported command parsing mode" >> pure Nothing

parserPrefs = prefs (disambiguate <> showHelpOnError <> showHelpOnEmpty)
