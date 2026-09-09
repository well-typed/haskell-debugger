{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Development.Debug.Adapter.Stepping
  ( commandContinue
  , commandNext
  , commandStepIn
  , commandStepOut
  ) where

import DAP

import GHC.Debugger.Interface.Messages hiding (Command, Response)

import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import Development.Debug.Adapter.Evaluation

commandContinue :: DebugAdaptor ()
commandContinue = do
  ContinueArguments {..} <- getArguments
  let rtw = bool_single_thrd continueArgumentsSingleThread
  let rsm = DoResume (RemoteThreadId continueArgumentsThreadId) ResumeNoStep rtw
  resetObjectReferences
  DidResume er <- sendInterleaved rsm $
    sendContinueResponse (ContinueResponse (maybe True not continueArgumentsSingleThread))
  handleEvalResult False er

commandNext :: DebugAdaptor ()
commandNext = do
  NextArguments {..} <- getArguments
  let rtw = bool_single_thrd nextArgumentsSingleThread
  let rsm = DoResume (RemoteThreadId nextArgumentsThreadId) ResumeStepLocal rtw
  resetObjectReferences
  DidResume er <- sendInterleaved rsm sendNextResponse
  handleEvalResult True er

commandStepIn :: DebugAdaptor ()
commandStepIn = do
  StepInArguments {..} <- getArguments
  let rtw = bool_single_thrd stepInArgumentsSingleThread
  let rsm = DoResume (RemoteThreadId stepInArgumentsThreadId) ResumeSingleStep rtw
  resetObjectReferences
  DidResume er <- sendInterleaved rsm sendStepInResponse
  handleEvalResult True er

commandStepOut :: DebugAdaptor ()
commandStepOut = do
  StepOutArguments {..} <- getArguments
  let rtw = bool_single_thrd stepOutArgumentsSingleThread
  let rsm = DoResume (RemoteThreadId stepOutArgumentsThreadId) ResumeStepOut rtw
  resetObjectReferences
  DidResume er <- sendInterleaved rsm sendStepOutResponse
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

bool_single_thrd :: Maybe Bool -> ResumeTheWorld
bool_single_thrd = bool_mby ResumeJustThisThread ResumeTheWorld

bool_mby :: a {- if Just True -} -> a {- if Nothing or Just False -} -> Maybe Bool -> a
bool_mby l r = \case
  Nothing    -> r
  Just False -> r
  Just True  -> l
