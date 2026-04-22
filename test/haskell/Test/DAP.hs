{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Test.DAP
  ( module Test.DAP
  , module Test.DAP.Init
  , module Test.DAP.Messages
  ) where

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Control.Monad.Reader
import Test.DAP.Init
import Test.DAP.Messages
import Test.DAP.Messages.Parser
import Control.Concurrent.Async
import Control.Monad.Cont
import Control.Monad
import Data.Maybe
import qualified Data.HashMap.Strict as H
import System.FilePath
import qualified Data.Text as T
import Data.Aeson.Types
import Test.Tasty.HUnit
import DAP (ScopesArguments(..), StackTraceArguments(..), DisconnectArguments(..))
import qualified DAP
import Test.DAP.Orphans ()
import DAP.Types

import qualified Data.List as List

--------------------------------------------------------------------------------
-- * DSL
--------------------------------------------------------------------------------
-- fill as needed; some parts of the highest level DSL will prefer NOT to be sync.

next, stepIn :: TestDAP ()
next   = void . sync $ nextRequest @Value @Value Null
stepIn = void . sync $ stepInRequest @Value @Value Null

threads :: TestDAP [Thread]
threads = do
  Response{responseBody=Just ThreadsResponse{threads=ts}} <- sync threadsRequest
  return ts

stackTrace :: Int -> TestDAP [StackFrame]
stackTrace tid = do
  Response{responseBody=Just StackTraceResponse{stackFrames=fs}} <- sync $ stackTraceRequest $
    StackTraceArguments
      { DAP.stackTraceArgumentsThreadId = tid
      , DAP.stackTraceArgumentsStartFrame = Nothing
      , DAP.stackTraceArgumentsLevels = Nothing
      , DAP.stackTraceArgumentsFormat = Nothing
      }
  return fs

scopes :: Int {- stack frame id -} -> TestDAP [Scope]
scopes frameId = do
  Response{responseBody=Just ScopesResponse{scopes=scs}} <- sync $ scopesRequest $
    ScopesArguments { DAP.scopesArgumentsFrameId = frameId }
  return scs

configurationDone :: ResponseCont Value a -> TestDAP a
configurationDone = configurationDoneRequest Nothing

--------------------------------------------------------------------------------
-- ** "Scenarios"
--------------------------------------------------------------------------------

-- | Register handler that will reply to runInTerminal reverse request
handleRunInTerminal :: AsyncCont (Maybe (H.HashMap T.Text T.Text), [T.Text]) (a, Int)
                    -- ^ Continuation receives async with args of runInTerm req.
                    --
                    -- - Waiting means block waiting for reverse request to be received
                    -- - Returns the process id of the launched runInTerminal process.
                    -> TestDAP a
handleRunInTerminal k = do
  ctx <- ask
  liftIO $
    withAsync (upd <$> runTestDAP waitForReverseRequest ctx) $ \a ->
      runTestDAP (k a >>= \(x, pid) -> do
        respondWithBody 0{-hardcode seqn-} "runInTerminal" (object [ "shellProcessId" .= pid ])
        return x
        ) ctx
  where
    upd :: Value -> (Maybe (H.HashMap T.Text T.Text), [T.Text])
    upd orig =
      fromMaybe (error $ "Failed to parse runInTerminal request: " ++ show orig) $
      parseMaybe parser orig

    parser :: Value -> Parser (Maybe (H.HashMap T.Text T.Text), [T.Text])
    parser = withObject "runInTerminal request" $ \o -> do
      ("request" :: String) <- o .: "type"
      ("runInTerminal" :: String) <- o .: "command"
      argsObj <- o .: "arguments"
      withObject "runInTerminal arguments" (\a -> do
        env :: Maybe (H.HashMap T.Text T.Text) <- a .:? "env"
        args :: [T.Text] <- a .: "args"
        pure (env, args)
        ) argsObj

-- | Launch a config, run to completion and assert the exit code.
runToEnd :: LaunchConfig -> TestDAP ()
runToEnd cfg = do
  _ <- sync $ launchWith cfg
  waitFiltering_ EventTy "initialized"
  _ <- sync configurationDone
  waitForExitCode 0

--------------------------------------------------------------------------------
-- ** Custom launch configs
--------------------------------------------------------------------------------

-- | Launch configuration for the debugger. Mirrors the NodeJS test config.
data LaunchConfig = LaunchConfig
  { lcProjectRoot :: FilePath
  , lcEntryFile :: Maybe FilePath
  , lcEntryPoint :: Maybe String
  , lcEntryArgs :: [String]
  , lcExtraGhcArgs :: [String]
  , lcInternalInterpreter :: Maybe Bool
  }

-- | A launch config with the given project root and entry file. @entryPoint@
-- defaults to @main@ and there are no extra args.
mkLaunchConfig :: FilePath -> FilePath -> LaunchConfig
mkLaunchConfig projectRoot entryFile = LaunchConfig
  { lcProjectRoot = projectRoot
  , lcEntryFile = Just entryFile
  , lcEntryPoint = Just "main"
  , lcEntryArgs = []
  , lcExtraGhcArgs = []
  , lcInternalInterpreter = Nothing
  }

-- | Launch the debugger with a 'LaunchConfig'.
launchWith :: LaunchConfig -> ResponseCont Value a -> TestDAP a
launchWith LaunchConfig{..} = launch $ object $
  [ "projectRoot" .= lcProjectRoot
  , "request" .= ("launch" :: String)
  ] ++
  [ "entryFile" .= (lcProjectRoot </> ef) | Just ef <- [lcEntryFile] ] ++
  [ "entryPoint" .= ep | Just ep <- [lcEntryPoint] ] ++
  [ "entryArgs" .= lcEntryArgs ] ++
  [ "extraGhcArgs" .= lcExtraGhcArgs ] ++
  [ "internalInterpreter" .= b | Just b <- [lcInternalInterpreter] ]

-- | Set breakpoints in a particular source file of a project at the given
-- lines.
setLineBreakpoints :: FilePath -- ^ project root
                   -> FilePath -- ^ entry file (relative to project root)
                   -> [Int]
                   -> ResponseCont Value a -> TestDAP a
setLineBreakpoints projectRoot entryFile lines_ =
  setBreakpointsIn projectRoot entryFile
    [ defaultSourceBreakpoint { sourceBreakpointLine = l } | l <- lines_ ]

-- | Set breakpoints (with arbitrary 'SourceBreakpoint's) in a particular
-- source file of a project.
setBreakpointsIn :: FilePath -- ^ project root
                 -> FilePath -- ^ source file (relative to project root)
                 -> [SourceBreakpoint]
                 -> ResponseCont Value a -> TestDAP a
setBreakpointsIn projectRoot entryFile bps =
  setBreakpointsRequest DAP.SetBreakpointsArguments
    { DAP.setBreakpointsArgumentsSource = DAP.defaultSource
        { DAP.sourceName = Just (T.pack (takeFileName entryFile))
        , DAP.sourcePath = Just (T.pack (projectRoot </> entryFile))
        }
    , DAP.setBreakpointsArgumentsBreakpoints = Just bps
    , DAP.setBreakpointsArgumentsLines = Just [sourceBreakpointLine bp | bp <- bps]
    , DAP.setBreakpointsArgumentsSourceModified = Just False
    }

-- | Launch, configure breakpoint, hit breakpoint.
hitBreakpointWith :: LaunchConfig -> Int -> TestDAP ()
hitBreakpointWith cfg@LaunchConfig{..} line = do
  entryFile <- maybe (fail "hitBreakpointWith: missing entryFile") pure lcEntryFile
  hitBreakpointIn cfg entryFile line

-- | Launch, configure a breakpoint in the given source file (relative to
-- the project root) at the given line, then wait for it to be hit.
hitBreakpointIn :: LaunchConfig -> FilePath -> Int -> TestDAP ()
hitBreakpointIn cfg@LaunchConfig{..} bpFile line = do
  _ <- sync $ launchWith cfg
  waitFiltering_ EventTy "initialized"
  _ <- sync $ setLineBreakpoints lcProjectRoot bpFile [line]
  _ <- sync configurationDone
  _ <- assertStoppedLocation DAP.StoppedEventReasonBreakpoint line
  return ()

--------------------------------------------------------------------------------
-- ** Variable inspection
--------------------------------------------------------------------------------
data VarsView = VarsView
  { varsViewDesc :: String
  , varsViewVars :: M.Map T.Text [Variable] }

mkVarsView :: String -> [Variable] -> VarsView
mkVarsView ctxDesc vs = VarsView
  { varsViewDesc = ctxDesc
  , varsViewVars = M.fromListWith (++) [(variableName v, [v]) | v <- vs] }

-- | Get variable by name
(%) :: VarsView -> String -> Variable
(%) vv n = case vv %% n of
  [v] -> v
  what -> error $ "Unexpected *many* variables by name " ++ show n ++ ": " ++ show what

-- | Get variables by name (there may be more than one with the same name; e.g. consider punning)
(%%) :: VarsView -> String -> [Variable]
(%%) VarsView{..} n
  | Just vs <- M.lookup (T.pack n) varsViewVars = vs
  | otherwise = error $
      "Variable " ++ show n ++ " not found in " ++ varsViewDesc ++ ": " ++ show (M.keys varsViewVars)
--------------------------------------------------------------------------------
-- | Fetch all variables from the scope with the given name in the top-most
-- frame of the first thread.
fetchScopeVars :: T.Text -> TestDAP VarsView
fetchScopeVars scopeName_ = do
  Response{responseBody=Just ThreadsResponse{threads=t:_}} <- sync threadsRequest
  Response{responseBody=Just StackTraceResponse{stackFrames=fr:_}} <- sync $ stackTraceRequest $
    StackTraceArguments
      { DAP.stackTraceArgumentsThreadId = threadId t
      , DAP.stackTraceArgumentsStartFrame = Nothing
      , DAP.stackTraceArgumentsLevels = Nothing
      , DAP.stackTraceArgumentsFormat = Nothing
      }
  Response{responseBody=Just ScopesResponse{scopes=scs}} <- sync $ scopesRequest $
    ScopesArguments { DAP.scopesArgumentsFrameId = stackFrameId fr }
  case List.find ((== scopeName_) . scopeName) scs of
    Nothing -> liftIO $ assertFailure $
      "fetchScopeVars: scope " ++ show scopeName_
        ++ " not found (" ++ show (map scopeName scs) ++ ")"
    Just sc -> do
      vs <- fetchChildren (scopeVariablesReference sc)
      pure $ mkVarsView ("scope " ++ show scopeName_) vs

fetchLocalVars :: TestDAP VarsView
fetchLocalVars = fetchScopeVars "Locals"

fetchModuleVars :: TestDAP VarsView
fetchModuleVars = fetchScopeVars "Module"

-- | Get the children of a variable by its variablesReference.
fetchChildren :: Int -> TestDAP [Variable]
fetchChildren ref = do
  Response{responseBody=Just (VariablesResponse vs)} <- sync $ variablesRequest $
    VariablesArguments
      { DAP.variablesArgumentsVariablesReference = ref
      , DAP.variablesArgumentsFilter = Nothing
      , DAP.variablesArgumentsStart = Nothing
      , DAP.variablesArgumentsCount = Nothing
      , DAP.variablesArgumentsFormat = Nothing
      }
  return vs

-- | Force a lazy variable by sending a 'variables' request for its reference
-- and returning the first child.
forceLazy :: HasCallStack => Variable -> TestDAP Variable
forceLazy v = do
  liftIO $ do
    assertEqual ("Variable " ++ show (variableName v) ++ " should be lazy")
      (Just True) (variablePresentationHintLazy =<< variablePresentationHint v)
    assertEqual ("Variable " ++ show (variableName v) ++ " should be \"_\" because it is lazy") "_" (variableValue v)
    assertBool ("Variable " ++ show (variableName v) ++ " should be expandable (because it is a lazy var)") $
      variableVariablesReference v /= 0 -- if it is expandable we get a reference to use to expand here

  -- Force the lazy variable by using its reference
  vs <- fetchChildren (variableVariablesReference v)
  case vs of
    (v':_) -> pure v'
    [] -> liftIO $ assertFailure $ "forceLazy: no children for forced variable " ++ show (variableName v)

-- | Expand a structured variable.
expandVar :: HasCallStack => Variable -> TestDAP VarsView
expandVar v = do
  liftIO $
    assertBool ("Variable " ++ show (variableName v) ++ " should be expandable (it should have structure)") $
      variableVariablesReference v /= 0
  vs <- fetchChildren (variableVariablesReference v)
  pure $ mkVarsView ("children of " ++ T.unpack (variableName v)) vs

-- | Assert that a variable has a given String value.
assertIsString :: HasCallStack => Variable -> T.Text -> TestDAP ()
assertIsString v expected = liftIO $ do
  assertEqual ("Variable " ++ show (variableName v) ++ " should be a String")
    (Just "String") (variableType v)
  assertEqual ("Variable " ++ show (variableName v) ++ " should be " ++ show expected)
    expected (variableValue v)
  assertEqual ("Variable " ++ show (variableName v) ++ " should not be expandable (because it is a String)")
    0 (variableVariablesReference v)

-- | Assert the value of some variable.
-- e.g. @var \@==? "\"hello\""@
(@==?) :: HasCallStack => Variable -> T.Text -> TestDAP ()
(@==?) v expected = liftIO $ assertEqual (T.unpack (variableValue v) ++ " @==? " ++ T.unpack expected) expected (variableValue v)

--------------------------------------------------------------------------------
-- ** Exception info & other requests
--------------------------------------------------------------------------------

exceptionInfo :: Int -> TestDAP ExceptionInfoResponse
exceptionInfo tid = do
  Response{responseBody=Just r} <- sync $ exceptionInfoRequest $
    ExceptionInfoArguments { DAP.exceptionInfoArgumentsThreadId = tid }
  return r

setBreakOnException :: TestDAP ()
setBreakOnException = do
  _ <- sync $ setExceptionBreakpointsRequest @_ @Value
    SetExceptionBreakpointsArguments
      { DAP.setExceptionBreakpointsArgumentsFilters = ["break-on-exception"]
      , DAP.setExceptionBreakpointsArgumentsFilterOptions = Nothing
      , DAP.setExceptionBreakpointsArgumentsExceptionOptions = Nothing
      }
  pure ()

continueThread :: Int -> TestDAP ()
continueThread tid = do
  _ <- sync $ continueRequest @_ @Value
    ContinueArguments
      { DAP.continueArgumentsThreadId = tid
      , DAP.continueArgumentsSingleThread = False
      }
  pure ()

evaluate :: T.Text -> TestDAP EvaluateResponse
evaluate expr = do
  Response{responseBody=Just r} <- sync $ evaluateRequest $
    EvaluateArguments
      { DAP.evaluateArgumentsExpression = expr
      , DAP.evaluateArgumentsFrameId = Nothing
      , DAP.evaluateArgumentsContext = Nothing
      , DAP.evaluateArgumentsFormat = Nothing
      }
  return r

stepOut :: Int -> TestDAP ()
stepOut tid = do
  _ <- sync $ stepOutRequest @_ @Value
    StepOutArguments
      { DAP.stepOutArgumentsThreadId = tid
      , DAP.stepOutArgumentsSingleThread = False
      , DAP.stepOutArgumentsGranularity = Nothing
      }
  pure ()

-- | Wait for an "exited" event and assert the exit code.
waitForExitCode :: Int -> TestDAP ()
waitForExitCode expected = do
  Event{eventBody = Just ExitedEvent{exitedEventExitCode}} <- waitFiltering EventTy "exited"
  liftIO $ assertEqual ("exit code should be " ++ show expected) expected exitedEventExitCode

disconnect :: TestDAP ()
disconnect = do
  Response{responseSuccess} <- sync $ disconnectRequest @_ @Value $ Just
    DisconnectArguments
      { DAP.disconnectArgumentsRestart = False
      , DAP.disconnectArgumentsTerminateDebuggee = True
      , DAP.disconnectArgumentsSuspendDebuggee = False
      }
  liftIO $ assertBool "disconnect response should indicate success" responseSuccess
  return ()

--------------------------------------------------------------------------------
-- ** Convenience methods (based on vscode-debugadapter-node/testSupport)
--------------------------------------------------------------------------------

launch :: Value {-^ Launch args -} -> ResponseCont Value a -> TestDAP a
launch args = runContT $
  ContT initializeRequest
    >>= liftIO . wait
    >> ContT (launchRequest args)

configurationSequence :: ResponseCont Value a -> TestDAP a
configurationSequence k = do
  waitFiltering_ EventTy "initialized"
  configurationDone k

-- | Assert that a "stopped" event with the given reason is received, then
-- fetch the stack trace and assert the top frame is at the expected line.
assertStoppedLocation :: HasCallStack => DAP.StoppedEventReason -> Int -> TestDAP ()
assertStoppedLocation reason expectedLine = do
  Event{eventBody = Just StoppedEvent{stoppedEventReason, stoppedEventThreadId}} <- waitFiltering EventTy "stopped"
  liftIO $ assertBool "Stopped reason matches expected reason" (stoppedEventReason == reason)
  frames <- maybe (pure []) stackTrace stoppedEventThreadId
  case frames of
    (frame:_) -> liftIO $ assertEqual "stopped location: line mismatch" expectedLine (stackFrameLine frame)
    []        -> liftIO $ assertFailure "assertStoppedLocation: no stack frames"

-- | Assert that all *pending*(not yet taken from channel) accumulated output
-- events (until any other event is found) contain a certain string
assertOutput :: HasCallStack => T.Text -> TestDAP ()
assertOutput expected = do
  events <- waitAccumulating EventTy "output"
  let outputs = map (outputEventOutput . fromMaybe (error "assertOutput:fromMaybe") . eventBody) events
  liftIO $
    assertBool
      ("assertOutput: expecting " ++ show expected ++ " but got " ++ show outputs)
      (any (T.isInfixOf expected) outputs)

assertFullOutput :: HasCallStack => T.Text -> TestDAP ()
assertFullOutput expected = assertFullOutputWith (expected <> " \\in full_output") (T.isInfixOf expected)

assertNotFullOutput :: HasCallStack => T.Text -> TestDAP ()
assertNotFullOutput expected = assertFullOutputWith (expected <> " \\notin full_output") (not . T.isInfixOf expected)

-- | Assert that the full output up until now matches any given string with the given function
assertFullOutputWith :: HasCallStack => T.Text -> (T.Text -> Bool) -> TestDAP ()
assertFullOutputWith assertStr test = do
  TestDAPClientContext{..} <- ask
  fullOut <- liftIO $ readTVarIO clientFullOutput
  liftIO $ assertBool
    ("assertFullOutputWith: asserting '" ++ show assertStr ++ "' but got " ++ show fullOut)
    (any test fullOut)

--------------------------------------------------------------------------------
-- * Waiting for messages
--------------------------------------------------------------------------------

data MsgType = EventTy | ResponseTy | ReverseRequestTy
  deriving Show

msgChan :: MsgType -> TestDAPClientContext -> TChan Value
msgChan ty TestDAPClientContext{..} = case ty of
  EventTy          -> clientEvents
  ResponseTy       -> clientResponses
  ReverseRequestTy -> clientReverseRequests

msgMatch :: MsgType -> String -> MessageMatch
msgMatch ty s = case ty of
  EventTy         -> eventMatch s
  ResponseTy      -> responseMatch s
  ReverseRequestTy -> reverseRequestMatch s

waitFiltering_ :: MsgType -> String -> TestDAP ()
waitFiltering_ ty s = void $ waitFiltering @Value ty s

-- | Drop messages of the given type until a message with the given
-- eventType/command is found. The matching message is returned.
-- FIXME: Timeouts on waiting, to avoid hanging forever in the testsuite!!
waitFiltering :: forall a. FromJSON a => MsgType -> String -> TestDAP a
waitFiltering ty s = do
  ch <- asks (msgChan ty)
  let mm = msgMatch ty s
  let loop = do
        v <- atomically $ readTChan ch -- block waiting for input
        if messageMatchMatches mm v
          then case fromJSON @a v of
            Error e -> error $ "waitFiltering: Failed to parse message MATCHING " ++ s ++ ":" ++ show ty ++ " with error: " ++ e ++ "\nFull message was: " ++ show v
            Success x -> return x
          else loop
  liftIO loop

-- | Accumulate messages of the given type until a message with a
-- eventType/command different from the given one is found.
--
-- The non-matching message is not consumed, nor returned, and will be kept in
-- the messages buffer.
waitAccumulating :: forall a. FromJSON a => MsgType -> String -> TestDAP [a]
waitAccumulating ty s = do
  ch <- asks (msgChan ty)
  let mm = msgMatch ty s
  let loop acc = do
        r <- atomically $ do
          v <- readTChan ch
          if messageMatchMatches mm v
            then case fromJSON @a v of
              Error e -> error $ "waitAccumulating: Failed to parse MATCHING message body with error: " ++ e ++ "\nFull message was: " ++ show v
              Success x -> pure (Just x)
            else Nothing <$ unGetTChan ch v
        case r of
          Nothing -> return (reverse acc)
          Just v  -> loop (v:acc)
  liftIO $ loop []

--------------------------------------------------------------------------------
-- * Protocol requests (based on vscode-debugadapter-node/testSupport)
--------------------------------------------------------------------------------

initializeRequest :: ResponseCont Value a -> TestDAP a
initializeRequest k = do
  TestDAPClientContext{clientSupportsRunInTerminal} <- ask
  customRequest "initialize"
    (Just $ object $
      [ "adapterID" .= ("haskell-debugger" :: String)
      , "clientID" .= ("mock-client" :: String)
      , "clientName" .= ("Mock Client" :: String)
      , "linesStartAt1" .= True
      , "columnsStartAt1" .= True
      , "locale" .= ("en" :: String)
      , "pathFormat" .= ("path" :: String)
      , "supportsRunInTerminalRequest" .= clientSupportsRunInTerminal
      ]) k

configurationDoneRequest :: Maybe Value -> ResponseCont Value a -> TestDAP a
configurationDoneRequest = customRequest "configurationDone"

launchRequest, attachRequest, restartRequest, setBreakpointsRequest,
  setFunctionBreakpointsRequest, setExceptionBreakpointsRequest,
  setInstructionBreakpointsRequest, dataBreakpointInfoRequest,
  setDataBreakpointsRequest, continueRequest, nextRequest, stepInRequest,
  stepOutRequest, stepBackRequest, reverseContinueRequest, restartFrameRequest,
  gotoRequest, pauseRequest, stackTraceRequest, scopesRequest, variablesRequest,
  setVariableRequest, sourceRequest, modulesRequest, evaluateRequest,
  disassembleRequest, stepInTargetsRequest, gotoTargetsRequest, completionsRequest,
  exceptionInfoRequest, readMemoryRequest, writeMemoryRequest :: (ToJSON a, FromJSON b) => a -> ResponseCont b r -> TestDAP r

launchRequest                    = requestWithArgs "launch"
attachRequest                    = requestWithArgs "attach"
restartRequest                   = requestWithArgs "restart"
setBreakpointsRequest            = requestWithArgs "setBreakpoints"
setFunctionBreakpointsRequest    = requestWithArgs "setFunctionBreakpoints"
setExceptionBreakpointsRequest   = requestWithArgs "setExceptionBreakpoints"
setInstructionBreakpointsRequest = requestWithArgs "setInstructionBreakpoints"
dataBreakpointInfoRequest        = requestWithArgs "dataBreakpointInfo"
setDataBreakpointsRequest        = requestWithArgs "setDataBreakpoints"
continueRequest                  = requestWithArgs "continue"
nextRequest                      = requestWithArgs "next"
stepInRequest                    = requestWithArgs "stepIn"
stepOutRequest                   = requestWithArgs "stepOut"
stepBackRequest                  = requestWithArgs "stepBack"
reverseContinueRequest           = requestWithArgs "reverseContinue"
restartFrameRequest              = requestWithArgs "restartFrame"
gotoRequest                      = requestWithArgs "goto"
pauseRequest                     = requestWithArgs "pause"
stackTraceRequest                = requestWithArgs "stackTrace"
scopesRequest                    = requestWithArgs "scopes"
variablesRequest                 = requestWithArgs "variables"
setVariableRequest               = requestWithArgs "setVariable"
sourceRequest                    = requestWithArgs "source"
modulesRequest                   = requestWithArgs "modules"
evaluateRequest                  = requestWithArgs "evaluate"
disassembleRequest               = requestWithArgs "disassemble"
stepInTargetsRequest             = requestWithArgs "stepInTargets"
gotoTargetsRequest               = requestWithArgs "gotoTargets"
completionsRequest               = requestWithArgs "completions"
exceptionInfoRequest             = requestWithArgs "exceptionInfo"
readMemoryRequest                = requestWithArgs "readMemory"
writeMemoryRequest               = requestWithArgs "writeMemory"

terminateRequest, disconnectRequest :: (ToJSON a, FromJSON b) => Maybe a -> ResponseCont b r -> TestDAP r
terminateRequest  = customRequest "terminate"
-- example:
--  object
--    [ "restart" .= False
--    , "terminateDebuggee" .= True
--    , "suspendDebuggee" .= False
--    ]
-- then...
-- wait "disconnected"
-- waitEventFiltering $ eventMatch "terminated"
disconnectRequest = customRequest "disconnect"

threadsRequest :: FromJSON b => ResponseCont b r -> TestDAP r
threadsRequest = customRequest "threads" (Nothing :: Maybe ())
--------------------------------------------------------------------------------
requestWithArgs :: (ToJSON a, FromJSON b) => String -> a -> ResponseCont b r -> TestDAP r
requestWithArgs command args = customRequest command (Just args)

-- example: respondWithBody revReqNum [ "shellProcessId" .= ... ]
respondWithBody :: Int -> String -> Value -> TestDAP ()
respondWithBody seqNum command body =
  reply seqNum $
    [ "type" .= ("response" :: String)
    , "command" .= command
    , "success" .= True
    , "body" .= body
    ]
--------------------------------------------------------------------------------
customRequest :: (ToJSON a, FromJSON b) => String -> Maybe a -> ResponseCont b r -> TestDAP r
customRequest command args = do
  send $
    [ "type" .= ("request" :: String)
    , "command" .= command
    ] ++ maybe [] (\v -> ["arguments" .= v]) args
