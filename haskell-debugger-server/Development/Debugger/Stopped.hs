{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}
-- | Getting information about where we're stopped at.
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
import System.FilePath

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
  root <- Development.Debugger.Adaptor.projectRoot <$> getDebugSession
  GotStacktrace fs <- sendSync GetStacktrace
  case fs of
    []  ->
      -- No frames; should be stopped on exception
      sendStackTraceResponse StackTraceResponse { stackFrames = [], totalFrames = Nothing }
    [f] -> do
      let
        fullFile = T.pack $
          if isAbsolute f.file then f.file else root </> f.file

        topStackFrame = defaultStackFrame
          { stackFrameId = 0
          , stackFrameName = T.pack f.name
          , stackFrameLine = f.startLine
          , stackFrameColumn = f.startCol
          , stackFrameEndLine = Just f.endLine
          , stackFrameEndColumn = Just f.endCol
          , stackFrameSource = Just defaultSource{sourcePath = Just fullFile}
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
  sendScopesResponse (ScopesResponse [])

--------------------------------------------------------------------------------
-- * Variables
--------------------------------------------------------------------------------
