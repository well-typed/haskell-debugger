{-# LANGUAGE RecordWildCards, OverloadedRecordDot, OverloadedStrings #-}

-- | Getting information about where we're stopped at (current suspended state).
--
-- Includes the commands to execute the following requests on the debuggee state:
-- 
-- @
-- Threads
--    StackTrace
--       Scopes
--          Variables
--             ...
--                Variables
-- @
module Development.Debugger.Stopped where

import qualified Data.Text as T

import DAP

import Debugger.Interface.Messages
import Development.Debugger.Adaptor
import Development.Debugger.Interface

--------------------------------------------------------------------------------
-- * StackTrace
--------------------------------------------------------------------------------

-- | Command to get thread information at current stopped point
commandThreads :: DebugAdaptor ()
commandThreads = do -- TODO
  sendThreadsResponse [
      Thread
        { threadId    = 0
        , threadName  = T.pack "dummy thread"
        }
    ]

--------------------------------------------------------------------------------
-- * StackTrace
--------------------------------------------------------------------------------

-- | Command to fetch stack trace at current stop point
commandStackTrace :: DebugAdaptor ()
commandStackTrace = do
  StackTraceArguments{..} <- getArguments
  GotStacktrace fs <- sendSync GetStacktrace
  case fs of
    []  ->
      -- No frames; should be stopped on exception
      sendStackTraceResponse StackTraceResponse { stackFrames = [], totalFrames = Nothing }
    [f] -> do
      source <- fileToSource f.sourceSpan.file
      let
        topStackFrame = defaultStackFrame
          { stackFrameId = 0
          , stackFrameName = T.pack f.name
          , stackFrameLine = f.sourceSpan.startLine
          , stackFrameColumn = f.sourceSpan.startCol
          , stackFrameEndLine = Just f.sourceSpan.endLine
          , stackFrameEndColumn = Just f.sourceSpan.endCol
          , stackFrameSource = Just source
          }
      sendStackTraceResponse StackTraceResponse
        { stackFrames = [topStackFrame]
        , totalFrames = Just 1
        }
    _ -> error $ "Unexpected multiple frames since implementation doesn't support it yet: " ++ show fs


--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Command to get scopes for current stopped point
commandScopes :: DebugAdaptor ()
commandScopes = do
  ScopesArguments{scopesArgumentsFrameId=0} <- getArguments
  GotScopes scopes <- sendSync GetScopes
  sendScopesResponse . ScopesResponse =<<
    mapM scopeInfoToScope scopes

-- | 'ScopeInfo' to 'Scope'
scopeInfoToScope :: ScopeInfo -> DebugAdaptor Scope
scopeInfoToScope ScopeInfo{..} = do
  source <- fileToSource sourceSpan.file
  return defaultScope
    { scopeName = case kind of
        LocalVariables -> "Locals"
        InteractiveVariables -> "Interactive"
        GlobalVariables -> "Globals"
        ReturnVariables -> "Returns"
    , scopePresentationHint = Just $ case kind of
        LocalVariables -> ScopePresentationHintLocals
        InteractiveVariables -> ScopePresentationHint "interactive"
        GlobalVariables -> ScopePresentationHint "globals"
        ReturnVariables -> ScopePresentationHint "returnValue"
    , scopeNamedVariables = numVars
    , scopeSource = Just source
    , scopeLine = Just sourceSpan.startLine
    , scopeColumn = Just sourceSpan.startCol
    , scopeEndLine = Just sourceSpan.endLine
    , scopeEndColumn = Just sourceSpan.endCol
    , scopeVariablesReference = fromEnum kind
    }

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------

commandVariables :: DebugAdaptor ()
commandVariables = do
  sendVariablesResponse (VariablesResponse [])

