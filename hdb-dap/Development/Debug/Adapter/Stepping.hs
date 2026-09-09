{-# LANGUAGE RecordWildCards #-}
module Development.Debug.Adapter.Stepping where

import DAP
import Data.Maybe

import GHC.Debugger.Interface.Messages hiding (Command, Response)

import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import Development.Debug.Adapter.Evaluation

commandContinue :: DebugAdaptor ()
commandContinue = do
  ContinueArguments {..} <- getArguments
  let mthread = if fromMaybe False continueArgumentsSingleThread
        then Just (RemoteThreadId continueArgumentsThreadId)
        else Nothing
  resetObjectReferences
  DidContinue er <- sendInterleaved (DoContinue mthread) $
    sendContinueResponse (ContinueResponse True)
  handleEvalResult False er

commandNext :: DebugAdaptor ()
commandNext = do
  NextArguments {..} <- getArguments
  let mthread = if fromMaybe False nextArgumentsSingleThread
        then Just (RemoteThreadId nextArgumentsThreadId)
        else error ("Nothing, but nextArgsThreadId: " ++ show nextArgumentsThreadId)-- Nothing
  resetObjectReferences
  DidStep er <- sendInterleaved (DoStepLocal mthread) sendNextResponse
  handleEvalResult True er

commandStepIn :: DebugAdaptor ()
commandStepIn = do
  StepInArguments {..} <- getArguments
  let mthread = if fromMaybe False stepInArgumentsSingleThread
        then Just (RemoteThreadId stepInArgumentsThreadId)
        else Nothing
  resetObjectReferences
  DidStep er <- sendInterleaved DoSingleStep sendStepInResponse
  handleEvalResult True er

commandStepOut :: DebugAdaptor ()
commandStepOut = do
  resetObjectReferences
  DidStep er <- sendInterleaved DoStepOut sendStepOutResponse
  handleEvalResult True er

--------------------------------------------------------------------------------

-- | See "Lifetime of Objects References" in DAP specification.
resetObjectReferences :: DebugAdaptor ()
resetObjectReferences = do
  updateDebugSession $ \s ->
    s { stackFrameMap = mempty
      , breakpointMap = mempty
      , variablesMap  = mempty
      }
