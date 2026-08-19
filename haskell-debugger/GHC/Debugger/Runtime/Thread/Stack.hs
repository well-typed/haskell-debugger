{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Decoding the stack of a thread at runtime
module GHC.Debugger.Runtime.Thread.Stack
  ( StackFrameInfo(..)
  , getRemoteThreadStackCopy
  ) where

import Control.Concurrent
import GHCi.RemoteTypes
import GHC.Debugger.Monad
import GHC.Debugger.Runtime.Interpreter.Types

#if MIN_VERSION_ghc(9,14,2)
import qualified GHC.Debugger.Runtime.Interpreter as Debuggee
#else
import qualified GHC.Debugger.Runtime.Interpreter.Legacy as Debuggee
#endif

--------------------------------------------------------------------------------
-- * Thread stack frames
--------------------------------------------------------------------------------

-- | Clone the stack of the given remote thread and get the breakpoint ids of available frames
getRemoteThreadStackCopy :: ForeignRef ThreadId -> Debugger [StackFrameInfo ForeignRef]
getRemoteThreadStackCopy = Debuggee.decodeThreadStack
