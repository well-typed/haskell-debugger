{-# LANGUAGE RecordWildCards, OverloadedRecordDot, OverloadedStrings, LambdaCase #-}

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
module Development.Debug.Adapter.Stopped where

import Control.Monad
import qualified Data.Text as T

import DAP

import GHC.Debugger.Interface.Messages
import Development.Debug.Adapter
import Development.Debug.Adapter.Interface

--------------------------------------------------------------------------------
-- * StackTrace
--------------------------------------------------------------------------------

-- | Command to get thread information at current stopped point
commandThreads :: DebugAdaptor ()
commandThreads = do
  GotThreads ts <- sendSync GetThreads
  sendThreadsResponse $
    map (\t ->
      Thread
        { threadId    = remoteThreadIntRef t.tId
        , threadName  = maybe (T.pack $ "Thread #" ++ show (remoteThreadIntRef t.tId)) T.pack t.tName
        }
      ) ts

--------------------------------------------------------------------------------
-- * StackTrace
--------------------------------------------------------------------------------

-- | Command to fetch stack trace at current stop point
commandStackTrace :: DebugAdaptor ()
commandStackTrace = do
  StackTraceArguments{..} <- getArguments
  GotStacktrace stackFrames <- sendSync (GetStacktrace (RemoteThreadId stackTraceArgumentsThreadId))
  responseFrames <- forM (zip stackFrames [1..]) $ \(stackFrame, stackFrameIx) -> do
    source <- fileToSource stackFrame.sourceSpan.file
    return defaultStackFrame
      { stackFrameId = stackTraceArgumentsThreadId*1000000 + stackFrameIx
      , stackFrameName = T.pack stackFrame.name
      , stackFrameLine = stackFrame.sourceSpan.startLine
      , stackFrameColumn = stackFrame.sourceSpan.startCol
      , stackFrameEndLine = Just stackFrame.sourceSpan.endLine
      , stackFrameEndColumn = Just stackFrame.sourceSpan.endCol
      , stackFrameSource = Just source
      }
  sendStackTraceResponse StackTraceResponse
    { stackFrames = responseFrames
    , totalFrames = if null responseFrames then Nothing else Just (length responseFrames) }

-- | Come up with a unique identifier for a stack frame (only valid while in
-- this stopped environment, see "Lifetime of Objects References" in DAP
-- overview).
--
-- The unique identifier is based on the threadId and index of the stack frame for this thread's stack.
-- mkUniqueStackId ::

--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Command to get scopes for current stopped point
commandScopes :: DebugAdaptor ()
commandScopes = do
  ScopesArguments{scopesArgumentsFrameId=_IGNORED_BUT_NOW_WE_HAVE_TO_CARE} <- getArguments
  GotScopes scopes <- sendSync GetScopes
  sendScopesResponse . ScopesResponse =<<
    mapM scopeInfoToScope scopes

-- | 'ScopeInfo' to 'Scope'
scopeInfoToScope :: ScopeInfo -> DebugAdaptor Scope
scopeInfoToScope ScopeInfo{..} = do
  source <- fileToSource sourceSpan.file
  return defaultScope
    { scopeName = case kind of
        LocalVariablesScope -> "Locals"
        ModuleVariablesScope -> "Module"
        GlobalVariablesScope -> "Globals"
    , scopePresentationHint = Just $ case kind of
        LocalVariablesScope -> ScopePresentationHintLocals
        ModuleVariablesScope -> ScopePresentationHint "module"
        GlobalVariablesScope -> ScopePresentationHint "globals"
    , scopeNamedVariables = numVars
    , scopeSource = Just source
    , scopeLine = Just sourceSpan.startLine
    , scopeColumn = Just sourceSpan.startCol
    , scopeEndLine = Just sourceSpan.endLine
    , scopeEndColumn = Just sourceSpan.endCol
    , scopeVariablesReference = fromEnum (scopeToVarRef kind)
    }

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------

-- | Command to get variables by reference number
commandVariables :: DebugAdaptor ()
commandVariables = do
  VariablesArguments{..} <- getArguments
  let vk = toEnum variablesArgumentsVariablesReference
  GotVariables vars <- sendSync (GetVariables vk)
  sendVariablesResponse $ VariablesResponse $
    map varInfoToVariables (either (:[]) id vars)
  case vars of
    -- If the reply indicates this was an "inspect lazy variable" request
    -- (because the requested variable was forced instead of returning an
    -- expansion), invalidate the parent variables.
    --
    -- The client side seems to handle rendering only the bits which changed
    -- out very well, while preserving the variable tree expansion.
    -- In any case, we might have to pessimistically redo all variable
    -- responses because any value may be changed by an updated thunk, not only
    -- the parent variables.
    Left _
      -> sendInvalidatedEvent defaultInvalidatedEvent
          { invalidatedEventAreas = [InvalidatedAreasVariables]
          , invalidatedEventStackFrameId = Just 0 -- we only support one stack frame for now (TODO).
          }
    _ -> return ()

-- | 'VarInfo' to 'Variable's.
--
-- Note that if 'VarInfo' is a nested structure, only the top-most VarInfo is
-- returned (with the according namedVariables and indexedVariables sizes).
--
-- The @'varFields'@ are ignored. If they are meant to be returned, they should
-- be matched against and returned explicitly (see @'getVariables'@).
varInfoToVariables :: VarInfo -> Variable
varInfoToVariables VarInfo{..} =
  defaultVariable
    { variableName = T.pack varName
    , variableValue = T.pack varValue
    , variableType = Just $ T.pack varType
    , variableEvaluateName = Just $ T.pack varName
    , variableVariablesReference = fromEnum varRef
    , variableNamedVariables = Nothing
    , variableIndexedVariables = Nothing
    , variablePresentationHint = Just defaultVariablePresentationHint
        { variablePresentationHintLazy = Just isThunk
        }
    }

