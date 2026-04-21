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

--------------------------------------------------------------------------------
-- * Highest level DSL
--------------------------------------------------------------------------------
-- fill as needed; some parts of the highest level DSL will prefer NOT to be sync.

defaultLaunch :: FilePath -> ResponseCont Value a -> TestDAP a
defaultLaunch testDir =
  launch $
    object
      [ "entryFile" .= (testDir </> "Main.hs")
      , "entryPoint" .= ("main" :: String)
      , "projectRoot" .= testDir
      , "extraGhcArgs" .= ([] :: [String])
      , "entryArgs" .= ([] :: [String])
      , "request" .= ("launch" :: String)
      ]

defaultSetBreakpoints :: FilePath -> [SourceBreakpoint] -> ResponseCont Value a -> TestDAP a
defaultSetBreakpoints testDir bps = do
  setBreakpointsRequest DAP.SetBreakpointsArguments
    { DAP.setBreakpointsArgumentsSource = DAP.defaultSource
        { DAP.sourceName = Just "Main.hs"
        , DAP.sourcePath = Just (T.pack (testDir </> "Main.hs"))
        }
    , DAP.setBreakpointsArgumentsBreakpoints = Just bps
    , DAP.setBreakpointsArgumentsLines = Just [sourceBreakpointLine bp | bp <- bps]
    , DAP.setBreakpointsArgumentsSourceModified = Just False
    }

defaultSetLineBreakpoints :: FilePath -> [Int] -> ResponseCont Value a -> TestDAP a
defaultSetLineBreakpoints testDir bps =
  defaultSetBreakpoints testDir
    [ defaultSourceBreakpoint { sourceBreakpointLine = line }
    | line <- bps
    ]

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

-- | Uses defaultLaunch and defaultSetBreakpoints
defaultHitBreakpoint :: FilePath -> Int -> TestDAP ()
defaultHitBreakpoint testDir line = do
  ctx <- ask
  liftIO $ mapConcurrently_
    (`runTestDAP` ctx)
    [ do waitFiltering_ EventTy "initialized"
         _ <- sync $ defaultSetLineBreakpoints testDir [line]
         _ <- sync $ configurationDone
         _ <- assertStoppedLocation DAP.StoppedEventReasonBreakpoint line
         return ()
    , void $ sync $ defaultLaunch testDir
    ]

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

-- | Assert that a "stopped" event with the given reason is received
assertStoppedLocation :: DAP.StoppedEventReason -> Int -> TestDAP ()
assertStoppedLocation reason expectedLine = do
  Event{eventBody = Just StoppedEvent{stoppedEventReason}} <- waitFiltering EventTy "stopped"
  liftIO $
    assertBool "Stopped reason matches expected reason" (stoppedEventReason == reason)
  -- TODO: Validate expected line and potentially path too
  -- (see assertStoppedLocation in debugClient.ts)

-- | Assert that all *pending*(not yet taken from channel) accumulated output
-- events (until any other event is found) contain a certain string
assertOutput :: T.Text -> TestDAP ()
assertOutput expected = do
  events <- waitAccumulating EventTy "output"
  let outputs = map (outputEventOutput . fromMaybe (error "assertOutput:fromMaybe") . eventBody) events
  liftIO $
    assertBool
      ("assertOutput: expecting " ++ show expected ++ " but got " ++ show outputs)
      (any (T.isInfixOf expected) outputs)

-- | Assert that the full output up until now contains any given string
assertFullOutput :: T.Text -> TestDAP ()
assertFullOutput expected = do
  TestDAPClientContext{..} <- ask
  fullOut <- liftIO $ readTVarIO clientFullOutput
  liftIO $ assertBool
    ("assertFullOutput: expecting " ++ show expected ++ " but got " ++ show fullOut)
    (any (T.isInfixOf expected) fullOut)

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
