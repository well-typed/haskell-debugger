module Development.Debug.Adapter.Stepping where

import DAP

import GHC.Debugger.Interface.Messages hiding (Command, Response)

import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import Development.Debug.Adapter.Evaluation

commandContinue :: DebugAdaptor ()
commandContinue = do
  resetObjectReferences
  DidContinue er <- sendInterleaved DoContinue $
    sendContinueResponse (ContinueResponse True)
  handleEvalResult False er

commandNext :: DebugAdaptor ()
commandNext = do
  resetObjectReferences
  DidStep er <- sendInterleaved DoStepLocal sendNextResponse
  handleEvalResult True er

commandStepIn :: DebugAdaptor ()
commandStepIn = do
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
