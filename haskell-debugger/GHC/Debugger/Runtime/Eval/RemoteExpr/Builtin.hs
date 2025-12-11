{-# LANGUAGE MagicHash #-}

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

import GHC.Exts
import Data.Word
import Foreign.C.String
import GHC.Conc.Sync
import GHC.Unit.Module
import GHC.Stack.CloneStack
import GHC.Exts.Heap
import GHC.Exts.Heap.Closures
import GHC.Debugger.Runtime.Eval.RemoteExpr (RemoteExpr)
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote

-- | Remote 'GHC.Stack.CloneStack.cloneThreadStack'
cloneThreadStack :: RemoteExpr ThreadId -> RemoteExpr (IO StackSnapshot)
cloneThreadStack = Remote.app $ Remote.var (mkModuleName "GHC.Stack.CloneStack") "cloneThreadStack" []

-- | Remote 'GHC.Stack.CloneStack.decode'
decode :: RemoteExpr StackSnapshot -> RemoteExpr (IO [StackEntry])
decode = Remote.app $ Remote.var (mkModuleName "GHC.Stack.CloneStack") "decode" []

-- | Remote 'GHC.Exts.Stack.decodeStack'
decodeStack :: RemoteExpr StackSnapshot -> RemoteExpr (IO StgStackClosure)
decodeStack = Remote.app $ Remote.var (mkModuleName "GHC.Exts.Stack") "decodeStack" []

-- | Remote 'GHC.Exts.Heap.Closures.ssc_stack'
ssc_stack :: RemoteExpr StgStackClosure -> RemoteExpr [StackFrame]
ssc_stack = Remote.app $ Remote.var (mkModuleName "GHC.Exts.Heap.Closures") "ssc_stack" []

-- | Remote 'GHC.Exts.Heap.getClosureData'
getClosureData :: RemoteExpr StgStackClosure -> RemoteExpr (IO Closure)
getClosureData = Remote.app $ Remote.var (mkModuleName "GHC.Exts.Heap") "getClosureData" ["GHC.Exts.LiftedRep", "_"]

-- | Remote 'GHC.Conc.Sync.fromThreadId'
fromThreadId :: RemoteExpr ThreadId -> RemoteExpr Word64
fromThreadId = Remote.app $ Remote.var (mkModuleName "GHC.Conc.Sync") "fromThreadId" []

-- | Remote 'GHC.Conc.Sync.threadStatus'
threadStatus :: RemoteExpr ThreadId -> RemoteExpr (IO ThreadStatus)
threadStatus = Remote.app $ Remote.var (mkModuleName "GHC.Conc.Sync") "threadStatus" []

-- | Remote 'GHC.Conc.Sync.listThreads'
listThreads :: RemoteExpr (IO [ThreadId])
listThreads = Remote.var (mkModuleName "GHC.Conc.Sync") "listThreads" []

-- | Remote 'GHC.Conc.Sync.threadLabel'
threadLabel :: RemoteExpr ThreadId -> RemoteExpr (IO (Maybe String))
threadLabel = Remote.app $ Remote.var (mkModuleName "GHC.Conc.Sync") "threadLabel" []

-- | Remote 'Foreign.C.String.peekCString'
peekCString :: RemoteExpr CString -> RemoteExpr (IO String)
peekCString = Remote.app $ Remote.var (mkModuleName "Foreign.C.String") "peekCString" []

-- | Remote 'GHC.Base.indexAddrArray#'
indexAddrArray :: RemoteExpr ByteArray# -> RemoteExpr (Int# -> Ptr a)
indexAddrArray = Remote.app $
  Remote.raw "\\b i -> GHC.Ptr.Ptr (GHC.Base.indexAddrArray# b i)"

-- | Remote 'Data.Maybe.maybeToList'
maybeToList :: RemoteExpr (Maybe a) -> RemoteExpr [a]
maybeToList = Remote.app $ Remote.var (mkModuleName "Data.Maybe") "maybeToList" []

-- | Function composition on the remote process
compose :: RemoteExpr (b -> c) -> RemoteExpr (a -> b) -> RemoteExpr (a -> c)
f `compose` g = Remote.app (Remote.app composeVar f) g where
  composeVar :: RemoteExpr ((b -> c) -> (a -> b) -> (a -> c))
  composeVar = Remote.var (mkModuleName "GHC.Base") "." []
