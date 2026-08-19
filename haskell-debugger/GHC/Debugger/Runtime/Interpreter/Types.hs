{-# LANGUAGE DeriveGeneric #-}
module GHC.Debugger.Runtime.Interpreter.Types where

import GHC.Generics (Generic)

import GHC.Debugger.Interface.Messages (DbgStackFrameBCOArgs)
import GHC.ByteCode.Types
import GHC.Conc.Sync
import GHC.InfoProv
import qualified GHC.Stack as Stack

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

data ThreadInfo ref = ThreadInfo
  { threadInfoRef    :: !(ref ThreadId)
  , threadInfoLabel  :: !(Maybe String)
  , threadInfoStatus :: !ThreadStatus
  }
  deriving (Generic)

-- | Information about a stack frame
data StackFrameInfo ref
  -- | Information derived from an IPE entry
  = StackFrameIPEInfo !InfoProv
  -- | User-defined Stack Frame annotation
  | StackFrameAnnotation !(Maybe Stack.SrcLoc) !String
  -- | Information derived from a continuation BCO breakpoint info.
  | StackFrameBreakpointInfo !InternalBreakpointId !(DbgStackFrameBCOArgs ref)
  deriving (Generic, Show)
