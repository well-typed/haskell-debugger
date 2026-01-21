{-# OPTIONS_GHC -Wno-orphans #-} -- TODO: drop this and Show GHC.InternalBreakpointId...
{-# LANGUAGE LambdaCase,
             StandaloneDeriving,
             DataKinds,
             OverloadedStrings,
             DuplicateRecordFields,
             TypeApplications
             #-}

-- | Types for sending and receiving messages to/from haskell-debugger
module GHC.Debugger.Interface.Messages where

import qualified GHC
import qualified GHC.Utils.Outputable as GHC

import GHC.Debugger.Runtime.Term.Key

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | The commands sent to the debugger
data Command

  -- | Set a breakpoint on a given function, or module by line number
  = SetBreakpoint { brk       :: Breakpoint
                  , hitCount  :: Maybe Int
                  -- ^ Stop after N hits (if @isJust condition@, count down only when @eval condition == True@)
                  , condition :: Maybe String
                  -- ^ Stop if condition evalutes to True
                  }

  -- | Delete a breakpoint on a given function, or module by line number
  | DelBreakpoint Breakpoint

  -- | Find the valid breakpoints locations for the given module Breakpoint
  | GetBreakpointsAt Breakpoint

  -- | Clear all breakpoints in the specified file.
  -- This is useful because DAP's `setBreakpoints` re-sets all breakpoints from zero for a source rather than incrementally.
  | ClearModBreakpoints { file :: FilePath }

  -- | Clear all function breakpoints
  | ClearFunctionBreakpoints

  -- | Get all threads
  | GetThreads

  -- | Get the evaluation stacktrace until the current breakpoint.
  | GetStacktrace RemoteThreadId

  -- | Get the list of available scopes at the current breakpoint
  | GetScopes RemoteThreadId Int

  -- | Get the variables in scope for the current breakpoint.
  --
  -- Note: for GHCs <9.16 this only reports the variables free in the expression
  -- we're stopped at rather than all variables in scope.
  | GetVariables RemoteThreadId Int{-stack frame positional ix-} VariableReference

  -- | Evaluate an expression at the current breakpoint.
  | DoEval String

  -- | Get information about the current exception (if any) on a thread.
  | GetExceptionInfo RemoteThreadId

  -- | Continue executing from the current breakpoint
  | DoContinue

  -- | Step local, which executes until next breakpoint in the same function.
  | DoStepLocal

  -- | Single step always to the next breakpoint. Used for "step-in".
  | DoSingleStep

  -- | Step out to the breakpoint immediately following a return.
  | DoStepOut

  -- | Execute a prog with debugging enabled. Breaks on the existing breakpoints.
  --
  -- Constructed with an entry point function name and the arguments to pass it.
  --
  -- When the @'EntryPoint'@ is @'Main'@, @'runArgs'@ are set as process
  -- invocation arguments (as in @argv@) rather than passed directly as a
  -- Haskell function arguments.
  | DebugExecution { entryPoint :: EntryPoint, entryFile :: FilePath, runArgs :: [String] }

  -- | Terminate haskell-debugger and exit
  | TerminateProcess

-- | An entry point for program execution.
data EntryPoint = MainEntry { mainName :: Maybe String } | FunctionEntry { fnName :: String }
  deriving (Show)

-- | A breakpoint can be set/removed on functions by name, or in modules by
-- line number. And, globally, for all exceptions, or just uncaught exceptions.
data Breakpoint
  = ModuleBreak { path :: FilePath, lineNum :: Int, columnNum :: Maybe Int }
  | FunctionBreak { function  :: String }
  | OnExceptionsBreak
  | OnUncaughtExceptionsBreak
  deriving (Show)

-- | Information about a scope
data ScopeInfo = ScopeInfo
      { kind :: ScopeVariablesReference
      , sourceSpan :: SourceSpan
      , numVars :: Maybe Int
      , expensive :: Bool }
  deriving (Show)

newtype VarFields = VarFields [VarInfo]

-- | Information about a variable
data VarInfo = VarInfo
      { varName  :: String
      , varType  :: String
      , varValue :: String
      , isThunk  :: Bool
      , varRef   :: VariableReference
      -- ^ A reference back to this variable

      -- TODO:
      --  memory reference using ghc-debug.
      }

-- | What kind of breakpoint are we referring to, module or function breakpoints?
-- Used e.g. in the 'ClearBreakpoints' request
data BreakpointKind
  -- | Module breakpoints
  = ModuleBreakpointKind
  -- | Function breakpoints
  | FunctionBreakpointKind
  deriving (Show, Eq)

instance GHC.Outputable BreakpointKind where ppr = GHC.text . show

-- | Referring to existing scopes
data ScopeVariablesReference
  = LocalVariablesScope
  | ModuleVariablesScope
  | GlobalVariablesScope
  deriving (Show, Eq, Ord)

-- | The type of variables referenced, or a particular variable referenced for its fields or value (when inspecting a thunk)
data VariableReference
  -- | A void reference to nothing at all. Used e.g. for ty cons and data cons
  = NoVariables

  -- | Variables in the local context (includes arguments, previous bindings)
  | LocalVariables

  -- | Variables in the module where we're stopped
  | ModuleVariables

  -- | Variables in the global context
  | GlobalVariables

  -- | A reference to a specific variable.
  -- Used to force its result or get its structured children
  | SpecificVariable TermKey

scopeToVarRef :: ScopeVariablesReference -> VariableReference
scopeToVarRef = \case
  LocalVariablesScope -> LocalVariables
  ModuleVariablesScope -> ModuleVariables
  GlobalVariablesScope -> GlobalVariables

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
      deriving (Show, Eq)

unhelpfulSourceSpan :: SourceSpan
unhelpfulSourceSpan = SourceSpan
  { file = ""
  , startLine = 0
  , endLine = 0
  , startCol = 0
  , endCol = 0
  }

--------------------------------------------------------------------------------
-- Responses
--------------------------------------------------------------------------------

-- | The responses sent by `haskell-debugger` to the client
data Response
  = DidEval EvalResult
  | DidSetBreakpoint BreakFound
  | DidRemoveBreakpoint BreakFound
  | DidGetBreakpoints (Maybe SourceSpan)
  | DidClearBreakpoints
  | DidContinue EvalResult
  | DidStep EvalResult
  | DidExec EvalResult
  | GotThreads [DebuggeeThread]
  | GotStacktrace [DbgStackFrame]
  | GotScopes [ScopeInfo]
  | GotVariables (Either VarInfo [VarInfo])
  | GotExceptionInfo ExceptionInfo
  | Aborted String
  | Initialised

data BreakFound
  = BreakFound
    { changed :: !Bool
    -- ^ Did the status of the found breakpoint change?
    , breakId :: [GHC.InternalBreakpointId]
    -- ^ Internal breakpoint identifier (module + ix) (TODO: Don't expose GHC)
    , sourceSpan :: SourceSpan
    -- ^ Source span for interface
    }
  -- | Breakpoint found but without location info.
  -- This happens when setting breakpoints on exceptions.
  | BreakFoundNoLoc
    { changed :: Bool }
  -- | No breakpoints found
  | BreakNotFound
  -- | Found many breakpoints.
  -- Caused by setting breakpoint on a name with multiple matches or many equations.
  | ManyBreaksFound [BreakFound]
  deriving (Show)

-- | A reference to a remote thread by remote id
-- See 'getRemoteThreadId'.
newtype RemoteThreadId = RemoteThreadId
    { remoteThreadIntRef :: Int
    -- ^ The number identifier of the thread on the (remote) interpreter. To
    -- find the proper remote 'ThreadId' corresponding to this numeric
    -- identifier, lookup the 'remoteThreadIntRef' in the 'ThreadMap'
    }
    deriving (Show, Eq, Ord)

data EvalResult
  = EvalCompleted { resultVal :: String
                  , resultType :: String
                  , resultStructureRef :: VariableReference
                  -- ^ A structured representation of the result of evaluating
                  -- the expression given as a "virtual" 'VariableReference'
                  -- that the user can use to refer to the result and inspect
                  -- interactively and expand it.
                  }
  | EvalException { resultVal :: String, resultType :: String }
  | EvalStopped   { breakId :: Maybe GHC.InternalBreakpointId
                  -- ^ Did we stop at an exception (@Nothing@) or at a breakpoint (@Just@)?
                  , breakThread :: RemoteThreadId
                  -- ^ In which thread did we hit the breakpoint?
                  }
  -- | Evaluation failed for some reason other than completed/completed-with-exception/stopped.
  | EvalAbortedWith String

data DebuggeeThread
  = DebuggeeThread
    { tId :: !RemoteThreadId
    -- ^ An identifier for a thread on the (possibly remote) debuggee process
    , tName :: !(Maybe String)
    -- ^ Thread label, if there is one
    }
    deriving (Show)

data DbgStackFrame
  = DbgStackFrame
    { name :: String
    -- ^ Title of stack frame
    , sourceSpan :: SourceSpan
    -- ^ Source span for this stack frame
    , breakId :: Maybe GHC.InternalBreakpointId
    -- ^ Is this a BCO continuation frame with a breakpoint?
    -- If yes, we can leverage the breakpoint info to report scopes.
    }
  deriving (Show)

data ExceptionInfo = ExceptionInfo
  { exceptionInfoTypeName     :: String
  , exceptionInfoFullTypeName :: String
  , exceptionInfoMessage      :: String
  , exceptionInfoContext      :: Maybe String
  , exceptionInfoInner        :: [ExceptionInfo]
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show GHC.InternalBreakpointId where
  show (GHC.InternalBreakpointId m ix) = "InternalBreakpointId " ++ GHC.showPprUnsafe m ++ " " ++ show ix
