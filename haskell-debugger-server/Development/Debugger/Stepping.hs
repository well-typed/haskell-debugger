module Development.Debugger.Stepping where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.IntSet as IS
import Control.Monad
import Data.Maybe

import qualified GHC

import DAP

import Debugger.Interface.Messages hiding (Command, Response)

import Development.Debugger.Adaptor
import Development.Debugger.Interface
import Development.Debugger.Evaluation

commandContinue :: DebugAdaptor ()
commandContinue = do
  DidContinue er <- sendInterleaved DoContinue $
    sendContinueResponse (ContinueResponse True)
  handleEvalResult False er

commandNext :: DebugAdaptor ()
commandNext = do
  DidStep er <- sendInterleaved DoStepLocal sendNextResponse
  handleEvalResult True er

commandStepIn :: DebugAdaptor ()
commandStepIn = do
  DidStep er <- sendInterleaved DoSingleStep sendStepInResponse
  handleEvalResult True er

commandStepOut :: DebugAdaptor ()
commandStepOut = undefined -- #6

