{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: We should be using ToJSON/FromJSON for all messages which `dap` has.
-- It's just we'd have to derive those instances as orphans or patch upstream.
-- (Upstream is better. Otherwise very long compile times)
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

defaultSetBreakpoints :: FilePath -> [(Int, Maybe String, Maybe String)] {- use `dap` types... -} -> ResponseCont Value a -> TestDAP a
defaultSetBreakpoints testDir bps = do
  setBreakpointsRequest $
    object
      [ "source" .= object
        [ "name" .= ("Main.hs" :: String)
        , "path" .= T.pack (testDir </> "Main.hs")
        ]
      , "breakpoints" .=
        [ object
          [ "line" .= line
          , "logMessage" .= logMessage
          , "condition" .= condition
          ]
        | (line,condition, logMessage) <- bps
        ]
      , "lines" .= [line | (line,_,_) <- bps ]
      , "sourceModified" .= False
      ]

defaultSetLineBreakpoints :: FilePath -> [Int] -> ResponseCont Value a -> TestDAP a
defaultSetLineBreakpoints testDir bps = defaultSetBreakpoints testDir [(b, Nothing, Nothing) | b <- bps]

next, stepIn :: TestDAP ()
next   = void . sync $ nextRequest Null
stepIn = void . sync $ stepInRequest Null

threads :: TestDAP [Int]
threads = do
  v <- sync threadsRequest
  return $ fromMaybe [] $ parseThreadIds v

stackTrace :: Int -> TestDAP [Int {-stack frame id-}]
stackTrace threadId = do
  v <- sync $ stackTraceRequest $ object [ "threadId" .= threadId ]
  return $ fromMaybe [] $ parseFramesIds v

-- | Request scopes and parse (name, expensive) pairs.
--
-- todo: please, use FromJSON/ToJSON of `dap` library datatypes. this is madness!
scopes :: Int {- stack frame id -} -> TestDAP [(String, Bool)]
scopes stackFrameId = do
  v <- sync $ scopesRequest $ object [ "frameId" .= stackFrameId ]
  case parseScopes v of
    Nothing -> fail $ "Could not parse scopes from: " ++ show v
    Just scs -> pure scs

configurationDone :: ResponseCont Value a -> TestDAP a
configurationDone = configurationDoneRequest Nothing

--------------------------------------------------------------------------------
-- ** "Scenarios"
--------------------------------------------------------------------------------

-- | Register handler that will reply to runInTerminal reverse request
handleRunInTerminal :: ResponseCont (Maybe (H.HashMap T.Text T.Text), [T.Text]) (a, Int)
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
    [ do _ <- waitFiltering Event "initialized"
         _ <- sync $ defaultSetLineBreakpoints testDir [line]
         _ <- sync $ configurationDone
         _ <- assertStoppedLocation "breakpoint" line
         return ()
    , void $ sync $ defaultLaunch testDir
    ]

-- waitEventFiltering $ eventMatch "terminated"
disconnect :: TestDAP ()
disconnect = do
  _ <- sync $ disconnectRequest $ Just $ object
    [ "restart" .= False
    , "terminateDebuggee" .= True
    , "suspendDebuggee" .= False
    ]
  _ <- waitFiltering Event "terminated"
  return ()

--------------------------------------------------------------------------------
-- ** Convenience methods (based on vscode-debugadapter-node/testSupport)
--------------------------------------------------------------------------------

launch :: Value {-^ Launch args -} -> ResponseCont Value a {- LaunchResponse, todo: use `dap` types w Aeson -} -> TestDAP a
launch args = runContT $
  ContT initializeRequest
    >>= liftIO . wait
    >> ContT (launchRequest args)

configurationSequence :: ResponseCont Value a -> TestDAP a
configurationSequence k = do
  _ <- waitFiltering Event "initialized"
  configurationDone k

-- | Assert that a "stopped" event with the given reason is received
assertStoppedLocation :: String -> Int -> TestDAP ()
assertStoppedLocation reason expectedLine = do
  -- TODO: Timeouts on waiting!!
  v <- waitFiltering Event "stopped"
  liftIO $
    assertBool "" (maybe False (==reason) (parseStoppedEventReason v))
  -- TODO: Validate expected line and potentially path too
  -- (see assertStoppedLocation in debugClient.ts)

-- | Assert that all *pending*(not yet taken from channel) accumulated output
-- events (until any other event is found) contain a certain string
assertOutput :: T.Text -> TestDAP ()
assertOutput expected = do
  events <- waitAccumulating Event "output"
  let outputs = mapMaybe parseOutput events
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

data MsgType = Event | Response | ReverseRequest

msgChan :: MsgType -> TestDAPClientContext -> TChan Value
msgChan ty TestDAPClientContext{..} = case ty of
  Event          -> clientEvents
  Response       -> clientResponses
  ReverseRequest -> clientReverseRequests

msgMatch :: MsgType -> String -> MessageMatch
msgMatch ty s = case ty of
  Event          -> eventMatch s
  Response       -> responseMatch s
  ReverseRequest -> reverseRequestMatch s

-- | Drop messages of the given type until a message with the given
-- eventType/command is found. The matching message is returned.
waitFiltering :: MsgType -> String -> TestDAP Value
waitFiltering ty s = do
  ch <- asks (msgChan ty)
  let mm = msgMatch ty s
  let loop = do
        v <- atomically $ readTChan ch -- block waiting for input
        if messageMatchMatches mm v
          then return v
          else loop
  liftIO loop

-- | Accumulate messages of the given type until a message with a
-- eventType/command different from the given one is found.
--
-- The non-matching message is not consumed, nor returned, and will be kept in
-- the messages buffer.
waitAccumulating :: MsgType -> String -> TestDAP [Value]
waitAccumulating ty s = do
  ch <- asks (msgChan ty)
  let mm = msgMatch ty s
  let loop acc = do
        r <- atomically $ do
          v <- readTChan ch
          if messageMatchMatches mm v
            then pure (Just v)
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
  exceptionInfoRequest, readMemoryRequest, writeMemoryRequest :: Value -> ResponseCont Value a -> TestDAP a

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

terminateRequest, disconnectRequest :: Maybe Value -> ResponseCont Value a -> TestDAP a
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

threadsRequest :: ResponseCont Value a -> TestDAP a
threadsRequest = customRequest "threads" Nothing
--------------------------------------------------------------------------------
requestWithArgs :: String -> Value -> ResponseCont Value a -> TestDAP a
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
customRequest :: String -> Maybe Value -> ResponseCont Value a -> TestDAP a
customRequest command args = do
  send $
    [ "type" .= ("request" :: String)
    , "command" .= command
    ] ++ maybe [] (\v -> ["arguments" .= v]) args
