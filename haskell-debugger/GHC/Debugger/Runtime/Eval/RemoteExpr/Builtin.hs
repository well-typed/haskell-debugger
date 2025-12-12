-- | A module providing various remote external variables which are available
-- by default in any session.
--
-- A good way to check whether some function or variable can be added here is
-- if you can successfully ask for the type of its fully qualified name on @ghci@
-- (e.g. @:t Data.List.singleton@)
--
-- Meant to be imported qualified @as Remote@:
-- @
-- import GHC.Debugger.Runtime.Eval.RemoteExpr (RemoteExpr)
-- import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
-- import qualified GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin as Remote
-- @
module GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin where

import GHC.Conc.Sync
import GHC.Unit.Module
import GHC.Stack.CloneStack
import GHC.Exts.Heap.Closures
import GHC.Debugger.Runtime.Eval.RemoteExpr (RemoteExpr)
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote

-- | Remote 'GHC.Stack.CloneStack.cloneThreadStack'
cloneThreadStack :: RemoteExpr (ThreadId -> IO StackSnapshot)
cloneThreadStack = Remote.var (mkModuleName "GHC.Stack.CloneStack") "cloneThreadStack"

-- | Remote 'GHC.Exts.Stack.decodeStack'
decodeStack :: RemoteExpr (StackSnapshot -> IO StgStackClosure)
decodeStack = Remote.var (mkModuleName "GHC.Exts.Stack") "decodeStack"

-- | Remote 'GHC.Exts.Heap.Closures.ssc_stack'
ssc_stack :: RemoteExpr (StgStackClosure -> [StackFrame])
ssc_stack = Remote.var (mkModuleName "GHC.Exts.Heap.Closures") "ssc_stack'"
