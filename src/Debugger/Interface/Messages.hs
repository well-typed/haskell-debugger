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

  -- | Clear all breakpoints in the specified file.
  -- This is useful because DAP's `setBreakpoints` re-sets all breakpoints from zero for a source rather than incrementally.
  | ClearModBreakpoints { file :: FilePath }

  -- | Clear all function breakpoints
  | ClearFunctionBreakpoints

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

  -- | Execute a prog with debugging enabled. Breaks on the existing breakpoints.
  --
  -- Constructed with an entry point function name and the arguments to pass it.
  --
  -- When the @'EntryPoint'@ is @'Main'@, @'runArgs'@ are set as process
  -- invocation arguments (as in @argv@) rather than passed directly as a
  -- Haskell function arguments.
  | DebugExecution { entryPoint :: EntryPoint, runArgs :: [String] }

  -- | Terminate ghc-debugger and exit
  | TerminateProcess

-- | An entry point for program execution.
data EntryPoint = MainEntry { mainName :: Maybe String } | FunctionEntry { fnName :: String }
  deriving (Show, Generic)

-- | A breakpoint can be set/removed on functions by name, or in modules by
-- line number. And, globally, for all exceptions, or just uncaught exceptions.
data Breakpoint
  = ModuleBreak { path :: FilePath, lineNum :: Int, columnNum :: Maybe Int }
  | FunctionBreak { function :: String }
  | OnExceptionsBreak
  | OnUncaughtExceptionsBreak
  deriving (Show, Generic)

-- | What kind of breakpoint are we referring to, module or function breakpoints?
-- Used e.g. in the 'ClearBreakpoints' request
data BreakpointKind
  -- | Module breakpoints
  = ModuleBreakpointKind
  -- | Function breakpoints
  | FunctionBreakpointKind
  deriving (Show, Generic, Eq)

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- | The responses sent by `ghc-debugger` to the client
data Response
  = DidEval EvalResult
  | DidSetBreakpoint BreakFound
  | DidRemoveBreakpoint BreakFound
  | DidClearBreakpoints
  | DidContinue EvalResult
  | DidStep EvalResult
  | DidExec EvalResult
  | StoppedEvent
  | Aborted String

data BreakFound
  = BreakFound
    { changed :: !Bool
    -- ^ Did the status of the found breakpoint change?
    , startLine :: {-# UNPACK #-} !Int
    -- ^ RealSrcSpan start line
    , endLine :: {-# UNPACK #-} !Int
    -- ^ RealSrcSpan end line
    , startCol :: {-# UNPACK #-} !Int
    -- ^ RealSrcSpan start col
    , endCol :: {-# UNPACK #-} !Int
    -- ^ RealSrcSpan end col
    }
  | BreakFoundNoLoc
    { changed :: Bool }
  deriving (Show, Generic)

data EvalResult
  = EvalCompleted { resultVal :: String, resultType :: String }
  | EvalException { resultVal :: String, resultType :: String }
  | EvalStopped   { exception :: Bool {-^ Did we stop at an exception (@True@) or at a breakpoint (@False@)? -} }
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
instance ToJSON BreakpointKind where toEncoding = genericToEncoding defaultOptions
instance ToJSON Response   where toEncoding = genericToEncoding defaultOptions
instance ToJSON EvalResult where toEncoding = genericToEncoding defaultOptions
instance ToJSON BreakFound where toEncoding = genericToEncoding defaultOptions
instance ToJSON EntryPoint where toEncoding = genericToEncoding defaultOptions

instance FromJSON Request
instance FromJSON Breakpoint
instance FromJSON BreakpointKind
instance FromJSON Response
instance FromJSON EvalResult
instance FromJSON BreakFound
instance FromJSON EntryPoint

