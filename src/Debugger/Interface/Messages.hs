{-# LANGUAGE DeriveGeneric,
             StandaloneDeriving,
             OverloadedStrings,
             DuplicateRecordFields
             #-}

-- | Types for sending and receiving messages to/from ghc-debugger
module Debugger.Interface.Messages where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary as B
import GHC.Generics
import Data.Aeson
import qualified GHC
import qualified GHC.Utils.Outputable as GHC
import GHC.Unit.Types () -- Binary

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | The commands sent to ghc debugger
data Command

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

  -- | Get the list of available scopes at the current breakpoint
  | GetScopes

  -- | Get the variables in scope for the current breakpoint.
  --
  -- Note: for GHCs <9.13 this only reports the variables free in the expression
  -- we're stopped at rather than all variables in scope.
  | GetVariables VariablesKind

  -- | Evaluate an expression at the current breakpoint.
  | DoEval String

  -- | Continue executing from the current breakpoint
  | DoContinue

  -- | Step local, which executes until next breakpoint in the same function.
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

-- | Information about a scope
data ScopeInfo = ScopeInfo
      { kind :: VariablesKind
      , sourceSpan :: SourceSpan
      , numVars :: Maybe Int
      , expensive :: Bool }
  deriving (Show, Generic)

-- | Information about a variable
data VarInfo = VarInfo
      { varName :: String
      , varType :: String
      , varValue :: String
      , isThunk :: Bool
      -- TODO:
      --  namedVariables
      --  indexedVariables
      -- TODO:
      --  memory reference using ghc-debug.
      }
      deriving (Show, Generic)

-- | What kind of breakpoint are we referring to, module or function breakpoints?
-- Used e.g. in the 'ClearBreakpoints' request
data BreakpointKind
  -- | Module breakpoints
  = ModuleBreakpointKind
  -- | Function breakpoints
  | FunctionBreakpointKind
  deriving (Show, Generic, Eq)

-- | The type of variables that are referred
data VariablesKind
  -- | Variables introduced in the interactive context (in the prompt)
  = InteractiveVariables
  -- | Variables in the local context (includes arguments, previous bindings)
  | LocalVariables
  -- | Variables in the global context (this may include interactive variables? FIXME)
  | GlobalVariables
  -- | Variables which will be bound when this expression returns (typically just @it@)
  | ReturnVariables

  -- TODO: DrilldownVariables VarId -> ...

  deriving (Show, Generic, Eq, Ord, Bounded, Enum)

-- | A source span type for the interface. Like 'RealSrcSpan'.
data SourceSpan = SourceSpan
      { file :: !FilePath
      -- ^ Path to file where this span is located
      , startLine :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan start line
      , endLine :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan end line
      , startCol :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan start col
      , endCol :: {-# UNPACK #-} !Int
      -- ^ RealSrcSpan end col
      }
      deriving (Show, Generic)

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
  | GotStacktrace [StackFrame]
  | GotScopes [ScopeInfo]
  | GotVariables [VarInfo]
  | Aborted String

data BreakFound
  = BreakFound
    { changed :: !Bool
    -- ^ Did the status of the found breakpoint change?
    , breakId :: GHC.BreakpointId
    -- ^ Internal breakpoint identifier (module + ix) (TODO: Don't expose GHC)
    , sourceSpan :: SourceSpan
    -- ^ Source span for interface
    }
  | BreakFoundNoLoc
    { changed :: Bool }
  deriving (Show, Generic)

data EvalResult
  = EvalCompleted { resultVal :: String, resultType :: String }
  | EvalException { resultVal :: String, resultType :: String }
  | EvalStopped   { breakId :: Maybe GHC.BreakpointId {-^ Did we stop at an exception (@Nothing@) or at a breakpoint (@Just@)? -} }
  deriving (Show, Generic)

data StackFrame
  = StackFrame
    { name :: String
    -- ^ Title of stack frame
    , sourceSpan :: SourceSpan
    -- ^ Source span for this stack frame
    }
  deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

deriving instance Show Command
deriving instance Generic Command

deriving instance Show Response
deriving instance Generic Response

instance ToJSON Command    where toEncoding = genericToEncoding defaultOptions
instance ToJSON Breakpoint where toEncoding = genericToEncoding defaultOptions
instance ToJSON BreakpointKind where toEncoding = genericToEncoding defaultOptions
instance ToJSON VariablesKind where toEncoding = genericToEncoding defaultOptions
instance ToJSON Response   where toEncoding = genericToEncoding defaultOptions
instance ToJSON EvalResult where toEncoding = genericToEncoding defaultOptions
instance ToJSON BreakFound where toEncoding = genericToEncoding defaultOptions
instance ToJSON SourceSpan where toEncoding = genericToEncoding defaultOptions
instance ToJSON EntryPoint where toEncoding = genericToEncoding defaultOptions
instance ToJSON StackFrame where toEncoding = genericToEncoding defaultOptions
instance ToJSON ScopeInfo  where toEncoding = genericToEncoding defaultOptions
instance ToJSON VarInfo    where toEncoding = genericToEncoding defaultOptions

instance FromJSON Command
instance FromJSON Breakpoint
instance FromJSON BreakpointKind
instance FromJSON VariablesKind
instance FromJSON Response
instance FromJSON EvalResult
instance FromJSON BreakFound
instance FromJSON SourceSpan
instance FromJSON EntryPoint
instance FromJSON StackFrame
instance FromJSON ScopeInfo
instance FromJSON VarInfo

instance Show GHC.BreakpointId where
  show (GHC.BreakpointId m ix) = "BreakpointId " ++ GHC.showPprUnsafe m ++ " " ++ show ix
instance ToJSON GHC.BreakpointId where
  toJSON (GHC.BreakpointId m ix) =
    undefined -- todo: why isn't Binary Module available here? it should exist
      -- In any case, we DO NOT want to expose GHC outside.
    -- object [ "module" .= BS.unpack (BS.toStrict (B.encode m))
    --        , "ix" .= ix
    --        ]
instance FromJSON GHC.BreakpointId where
  parseJSON = undefined -- for now, while developing with `dap` that needs no JSON

