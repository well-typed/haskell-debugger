{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, OverloadedRecordDot #-}
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
import Data.Maybe (fromJust)
import qualified Data.Maybe as Maybe
import GHC.Debugger.Debuggee (DebuggerLog)
import GHC.Debugger.Script

-- TODO: AST
newtype Command = Command String

data RunOptions = RunOptions
  { runEntryFile :: AbsFilePath
  , runEntryPoint :: String
  , runEntryArgs :: [String]
  , runProjectRoot :: AbsFilePath
  }

data RunContext = RunContext
  { runCurrentThread :: Maybe RemoteThreadId
  , runLastCommand :: Maybe Command
  }

-- | Interactive debugging monad
type InteractiveDM a = InputT (RWST RunOptions () RunContext Script) a

data InteractiveLog
  = IDebuggerLog DebuggerLog
  | ISessionSetupLog (WithSeverity SessionSetupLog)

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
runIDM logger runEntryPoint entryFile runEntryArgs extraGhcArgs cradleFile runConf act = do
  runProjectRoot <- mkAbsolute <$> getCurrentDirectory

  let hieBiosLogger = contramap ISessionSetupLog logger
  let runEntryFile = runProjectRoot /> entryFile

  entryFileExists <- doesFileExist (unAbs runEntryFile)
  when (not entryFileExists) $ do
    exitWithMsg $ "Entry file \"" ++ (unAbs runEntryFile) ++ "\" does not exist or is a directory."
  let
    runDbg m = do
      hieDebugRunner hieBiosLogger (DebugRunnerConf (unAbs runProjectRoot) entryFile extraGhcArgs cradleFile) >>= \case
        Left e               -> exitWithMsg e
        Right (_ghcInvocation, debugRunner)
                         -> do
          let debugRec = contramap IDebuggerLog logger

          runDebugger debugRec debugRunner runConf $
            m
  let
    runS :: (Debugger b0 -> IO b0) -> Script a -> IO a
    runS = undefined

  runS runDbg $ fmap fst $
          evalRWST (runInputT (setComplete noCompletion defaultSettings) act)
                   (RunOptions { runProjectRoot, runEntryFile, runEntryPoint, runEntryArgs })
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
              lift . lift $ interpretCmd cmd -- repeat last command
        Just input -> do
          mcmd <- parseCmd input
          case mcmd of
            Nothing -> return ()
            Just Exit -> outputStrLn "Exiting..." >> liftIO (exitWith ExitSuccess)
            Just (Do cmd) -> do
              lift $ modify (\ o -> o { runLastCommand = Just cmd })
              lift . lift $ interpretCmd cmd
      loop

interpretCmd :: Command -> Script ()
interpretCmd = _

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
parseCmd :: String -> InteractiveDM (Maybe (OrExit Command))
parseCmd input = do
  opts <- lift ask
  ctx <- lift get
  let
    res = case input of
      "exit" -> Success Exit
      s -> Success (Do $ Command s)
   in case res of
    Success cmd ->
      return (Just cmd)
    Failure bad ->
      let (msg, _exit) = renderFailure bad "(hdb)"
       in outputStrLn msg >> pure Nothing
    _ -> outputStrLn "Unsupported command parsing mode" >> pure Nothing

parserPrefs :: ParserPrefs
parserPrefs = prefs (disambiguate <> showHelpOnError <> showHelpOnEmpty)
