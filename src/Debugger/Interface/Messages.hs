{-# LANGUAGE DeriveGeneric,
             StandaloneDeriving
             #-}

-- | Types for sending and receiving messages to/from ghc-debugger
module Debugger.Interface.Messages where

import GHC.Generics
import Data.Aeson

--------------------------------------------------------------------------------
-- Requests
--------------------------------------------------------------------------------

-- | The requests sent by the client to `ghc-debugger`
data Request
-- TODO: DAPLAUNCH? DAPCONTEXTMODULES? DAPSCOPES?

  -- | Set a breakpoint on a given function, or module by line number
  = SetBreakpoint Breakpoint

  -- | Delete a breakpoint on a given function, or module by line number
  | DelBreakpoint Breakpoint

  -- | Clear all breakpoints.
  -- This is useful considering DAP re-sets all breakpoints from zero rather than incrementally.
  | ClearBreakpoints

  -- | Get the evaluation stacktrace until the current breakpoint.
  | GetStacktrace
  -- | Get the variables in scope for the current breakpoint.
  --
  -- Note: for GHCs <9.13 this only reports the variables free in the expression
  -- we're stopped at rather than all variables in scope.
  | GetVariables

  -- | Lists the source code for the current breakpoint.
  | GetSource

  -- | Evaluate an expression at the current breakpoint.
  | DoEval String

  -- | Continue executing from the current breakpoint
  | DoContinue
  -- | Step local, which executes until next breakpoint in the same function.
  -- Used for "step-next".
  | DoStepLocal
  -- | Single step always to the next breakpoint. Used for "step-in".
  | DoSingleStep

-- | A breakpoint can be set/removed on functions by name, or in modules by
-- line number. And, globally, for all exceptions, or just uncaught exceptions.
data Breakpoint
  = ModuleBreak { path :: FilePath, lineNum :: Int, columnNum :: Maybe Int }
  | FunctionBreak String
  | OnExceptionsBreak
  | OnUncaughtExceptionsBreak
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- | The responses sent by `ghc-debugger` to the client
data Response
  = DidEval EvalResult
  | DidSetBreakpoint Bool
  | DidRemoveBreakpoint Bool

data EvalResult
  = EvalCompleted { resultVal :: String, resultType :: String }
  | EvalException { resultVal :: String, resultType :: String }
  | EvalStopped
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

deriving instance Show Request
deriving instance Generic Request

deriving instance Show Response
deriving instance Generic Response

instance ToJSON Request    where toEncoding = genericToEncoding defaultOptions
instance ToJSON Breakpoint where toEncoding = genericToEncoding defaultOptions
instance ToJSON Response   where toEncoding = genericToEncoding defaultOptions
instance ToJSON EvalResult where toEncoding = genericToEncoding defaultOptions

instance FromJSON Request
instance FromJSON Breakpoint
instance FromJSON Response
instance FromJSON EvalResult

