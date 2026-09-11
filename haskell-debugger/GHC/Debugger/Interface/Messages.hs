{-# OPTIONS_GHC -Wno-orphans #-} -- TODO: drop this and Show GHC.InternalBreakpointId...
{-# LANGUAGE LambdaCase,
             StandaloneDeriving,
             DataKinds,
             OverloadedStrings,
             DuplicateRecordFields,
             TypeApplications
             #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Types for sending and receiving messages to/from haskell-debugger
module GHC.Debugger.Interface.Messages where

import qualified GHC
import qualified GHC.Utils.Outputable as GHC

import GHC.Debugger.Runtime.Term.Key
import Data.Binary (Binary)
import qualified GHC.Stack as Stack
import System.FilePath (isAbsolute, (</>), normalise)
import Control.Exception (assert)

{-
Note [Paths should be made absolute at the source]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We get `FilePath`s from a few different sources, and those sources do not always agree on what the paths should be relative to.

To avoid mistakes in interpreting relative paths, they should be made absolute as soon as we get them.

At the time of writing (25/07/2026), we handle paths from:
- CLI: relative to getCurrentDirectory.
- GHC: relative to getCurrentDirectory.
- DAP: relative to projectRoot.
- HIE: relative to projectRoot/workingDir, these are made absolute when creating DynFlags.
-}

-- | See Note [Paths should be made absolute at the source]
newtype AbsFilePath = MkAbsFilePath {unAbs :: FilePath}
  deriving newtype (Eq,Show)

mkAbsolute :: FilePath -> AbsFilePath
mkAbsolute fp = assert (null fp || isAbsolute fp) $ MkAbsFilePath fp

(/>) :: AbsFilePath -> FilePath -> AbsFilePath
MkAbsFilePath fp /> fp' = MkAbsFilePath $ normalise $ fp </> fp'

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
                  , logMessage :: Maybe String
                  -- ^ Log only if @condition@ (and @hitCondition@ when supported) are @True@.
                  }

  -- | Delete a breakpoint on a given function, or module by line number
  | DelBreakpoint Breakpoint

  -- | Find the valid breakpoints locations for the given module Breakpoint
  | GetBreakpointsAt Breakpoint

  -- | Clear all breakpoints in the specified file.
  -- This is useful because DAP's `setBreakpoints` re-sets all breakpoints from zero for a source rather than incrementally.
  | ClearModBreakpoints { file :: AbsFilePath }

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

  -- | Get information about the current exception (if any) on a thread.
  | GetExceptionInfo RemoteThreadId

  -- | Evaluate an expression at the current breakpoint.
  --
  -- When the frame id is given, the expression is run in the context of that frame.
  | DoEval (Maybe (RemoteThreadId, Int{-stack frame positional ix-})) String

  -- | Resume the paused thread using the given 'ResumeStep' and 'ResumeTheWorld' options.
  -- See the respective haddocks for details.
  --
  -- Examples:
  --
  -- - To "global continue": @DoResume <active tid> ResumeNoStep ResumeTheWorld@
  -- (in this case, the active tid is of no consequence, but still passed)
  --
  -- - To "single-thread step in": @DoResume <a tid> ResumeSingleStep ResumeJustThisThread@
  --
  -- - To "global step-next": @DoResume <a tid> ResumeStepLocal ResumeTheWorld@;
  --   All threads will be resumed and the specified thread will break at the
  --   next local breakpoint.
  | DoResume RemoteThreadId ResumeStep ResumeTheWorld

  -- | Execute a prog with debugging enabled. Breaks on the existing breakpoints.
  --
  -- Constructed with an entry point function name and the arguments to pass it.
  --
  -- When the @'EntryPoint'@ is @'Main'@, @'runArgs'@ are set as process
  -- invocation arguments (as in @argv@) rather than passed directly as a
  -- Haskell function arguments.
  | DebugExecution { entryPoint :: EntryPoint, entryFile :: AbsFilePath, runArgs :: [String] }

-- | The type of stepping to do when resuming a thread.
data ResumeStep
  = ResumeNoStep     -- ^ No stepping, i.e. just resume/continue the thread.
  | ResumeSingleStep -- ^ Single step, i.e. stop at the immediate next breakpoint
  | ResumeStepLocal  -- ^ Step to the next breakpoint on the same function at the source level
  | ResumeStepOut    -- ^ Step out to the case continuation to which the thread
                     -- returns from the current function

-- | Describes whether to resume all threads besides the specified one.
-- In the default stop-the-world mode, pause/continue should resume all threads
-- when we resume and pause all threads when we hit a breakpoint. When in
-- non-stop-the-world mode, only the specified thread gets resumed.
--
-- Important:
--
--   - When 'ResumeJustThisThread' is used, the debugger mode is toggled to
--     non-stop-the-world, so, when that thread hits a breakpoint, it is only
--     reported for that thread.
--
--   - When 'ResumeTheWorld' is used, the debugger mode is toggled to
--     stop-the-world (regardless of threads having just been resumed with
--     'ResumeJustThisThread'). From that point 'ResumeJustThisThread' is used
--     again, all breakpoints hit pause all threads in addition to the one that
--     hit the breakpoint.
data ResumeTheWorld
  -- | Resume only the specified thread and toggle the non-stop-the-world mode
  = ResumeJustThisThread
  -- | Resume all threads in addition to the specified one and toggle stop-the-world mode
  | ResumeTheWorld

-- | An entry point for program execution.
data EntryPoint = MainEntry { mainName :: Maybe String } | FunctionEntry { fnName :: String }
  deriving (Show)

-- | A breakpoint can be set/removed on functions by name, or in modules by
-- line number. And, globally, for all exceptions, or just uncaught exceptions.
data Breakpoint
  = ModuleBreak { path :: AbsFilePath, lineNum :: Int, columnNum :: Maybe Int }
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

-- | Result of requesting variables.
--
-- If the variable you requested is a thunk then `ForcedVariable` is returned, which
-- is the variable you requested but forced.
--
-- If the variable you requested is not a thunk, then 'VariableFields` is returned which
-- contains the subfields of the variable.
data VariableResult
  = ForcedVariable VarInfo
  | VariableFields [VarInfo]

variableResultToList :: VariableResult -> [VarInfo]
variableResultToList = \case
  ForcedVariable vi -> [vi]
  VariableFields vis -> vis

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
      { file :: !AbsFilePath
      -- ^ Path to file where this span is located.
      -- See Note [Paths should be made absolute at the source]
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

-- | This is a completely unhelpful source span!
-- It doesn't point to anything and is of no help to the user
-- whatsoever.
--
-- Use this only as a last resort if no other source span can be provided.
unhelpfulSourceSpan :: SourceSpan
unhelpfulSourceSpan = SourceSpan
  { file = mkAbsolute ""
  , startLine = 0
  , endLine = 0
  , startCol = 0
  , endCol = 0
  }

-- | See Note [Paths should be made absolute at the source]
srcLocToSourceSpan :: AbsFilePath -> Stack.SrcLoc -> SourceSpan
srcLocToSourceSpan prefix srcLoc =
  SourceSpan
    { file = prefix /> Stack.srcLocFile srcLoc
    , startLine = Stack.srcLocStartLine srcLoc
    , endLine = Stack.srcLocEndLine srcLoc
    , startCol = Stack.srcLocStartCol srcLoc
    , endCol = Stack.srcLocEndCol srcLoc
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
  | DidResume EvalResult
  | DidExec EvalResult
  | GotThreads [DebuggeeThread]
  | GotStacktrace [DbgStackFrame]
  | GotScopes [ScopeInfo]
  | GotVariables VariableResult
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
    deriving (Show, Eq, Ord, Binary)

data SourceKind = IsExpr | IsStmt

data EvalResult
  = EvalCompleted { resultVal :: String
                  , resultType :: String
                  , resultSourceKind :: Maybe SourceKind
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
  , exceptionInfoSourceSpan   :: Maybe SourceSpan
  , exceptionInfoInner        :: [ExceptionInfo]
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show GHC.InternalBreakpointId where
  show (GHC.InternalBreakpointId m ix) = "InternalBreakpointId " ++ GHC.showPprUnsafe m ++ " " ++ show ix
