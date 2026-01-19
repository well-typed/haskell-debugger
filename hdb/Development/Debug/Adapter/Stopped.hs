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
import qualified Data.IntMap as IM
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
  let threadId = RemoteThreadId stackTraceArgumentsThreadId
  GotStacktrace stackFrames <- sendSync (GetStacktrace threadId)
  (responseFrames, newStackFrameMap) <- fmap (unzip . concat) $
    forM (zip stackFrames [0..]) $ \(stackFrame, stackFrameIx) -> do
      freshId <- getFreshId
      source <- fileToSource stackFrame.sourceSpan.file
      let responseFrame = defaultStackFrame
            { stackFrameId = freshId
            , stackFrameName = T.pack stackFrame.name
            , stackFrameLine = stackFrame.sourceSpan.startLine
            , stackFrameColumn = stackFrame.sourceSpan.startCol
            , stackFrameEndLine = Just stackFrame.sourceSpan.endLine
            , stackFrameEndColumn = Just stackFrame.sourceSpan.endCol
            , stackFrameSource = Just source
            }
      let newMapEntry = (freshId, StackFrameIx threadId stackFrameIx)
      return [(responseFrame, newMapEntry)]

  updateDebugSession (\s -> s { stackFrameMap = s.stackFrameMap <> IM.fromList newStackFrameMap })

  sendStackTraceResponse StackTraceResponse
    { stackFrames = responseFrames
    , totalFrames = if null responseFrames then Nothing else Just (length responseFrames)
    }

--------------------------------------------------------------------------------
-- * Scopes
--------------------------------------------------------------------------------

-- | Command to get scopes for current stopped point
commandScopes :: DebugAdaptor ()
commandScopes = do
  ScopesArguments{..} <- getArguments
  let frameId = scopesArgumentsFrameId
  sfMap <- stackFrameMap <$> getDebugSession
  case IM.lookup frameId sfMap of
    Nothing -> do
      sendErrorResponse (ErrorMessage (T.pack $ "Could not find stack frame for id " ++ show frameId)) Nothing
    Just six@(StackFrameIx threadId frameIx) -> do
      GotScopes scopes <- sendSync (GetScopes threadId frameIx)
      sendScopesResponse . ScopesResponse =<<
        mapM (scopeInfoToScope six) scopes

-- | 'ScopeInfo' to 'Scope'
scopeInfoToScope :: StackFrameIx -> ScopeInfo -> DebugAdaptor Scope
scopeInfoToScope six ScopeInfo{..} = do

  -- Update vars map
  varId <- freshVarIx six (scopeToVarRef kind)

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
    , scopeVariablesReference = varId
    }

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------

-- | Command to get variables by reference number
commandVariables :: DebugAdaptor ()
commandVariables = do
  VariablesArguments{..} <- getArguments

  vsMap <- variablesMap <$> getDebugSession
  case IM.lookup variablesArgumentsVariablesReference vsMap of
    Nothing -> sendErrorResponse (ErrorMessage (T.pack $ "Could not find variable reference " ++ show variablesArgumentsVariablesReference)) Nothing
    Just (VariablesIx six@(StackFrameIx threadId frameIx) varRef) -> do
      GotVariables vars <- sendSync (GetVariables threadId frameIx varRef)
      sendVariablesResponse . VariablesResponse =<<
        mapM (varInfoToVariables six) (either (:[]) id vars)
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
              , invalidatedEventStackFrameId = Just 0 -- TODO: REVERSE LOOKUP OF (StackFrameIx threadId frameIx)
              }
        _ -> return ()

-- | 'VarInfo' to 'Variable's.
varInfoToVariables :: StackFrameIx -> VarInfo -> DebugAdaptor Variable
varInfoToVariables six VarInfo{..} = do
  varId <- freshVarIx six varRef

  return defaultVariable
    { variableName = T.pack varName
    , variableValue = T.pack varValue
    , variableType = Just $ T.pack varType
    , variableEvaluateName = Just $ T.pack varName
    , variableVariablesReference = varId
    , variableNamedVariables = Nothing
    , variableIndexedVariables = Nothing
    , variablePresentationHint = Just defaultVariablePresentationHint
        { variablePresentationHintLazy = Just isThunk
        }
    }

--------------------------------------------------------------------------------
-- Variable ix references
--------------------------------------------------------------------------------

freshVarIx :: StackFrameIx -> VariableReference -> DebugAdaptor Int
freshVarIx _ NoVariables = pure 0 -- No variables means the reference should be 0, e.g. denoting the variable is not expandable
freshVarIx six vr = do
  varId <- getFreshId
  updateDebugSession (\s -> s { variablesMap = IM.insert varId (VariablesIx six vr) s.variablesMap })
  return varId

