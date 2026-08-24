{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}

-- | Helpers used when the debugger is stopped due to an exception.
-- These helpers execute code on the remote process which teach us information
-- about the exception we are stopped at.
module GHC.Debugger.Stopped.Exception
  ( getExceptionInfo
  , defaultExceptionInfo
  ) where

import Data.Maybe

import GHC

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
  ( ExceptionInfo(..)
  , RemoteThreadId(..)
  )
import GHC.Debugger.Runtime.Thread
import GHCi.RemoteTypes
#if MIN_VERSION_ghc(9,14,2)
import qualified GHC.Debugger.Runtime.Interpreter as Debuggee
#else
import qualified GHC.Debugger.Runtime.Interpreter.Legacy as Debuggee
#endif
import Control.Exception (SomeException)

-- | Retrieve structured exception information for the requested thread when
-- the debugger is currently stopped on an exception.
getExceptionInfo :: RemoteThreadId -> Debugger ExceptionInfo
getExceptionInfo req_tid = GHC.getResumeContext >>= \case
  [] -> return defaultExceptionInfo
  r:_ -> do
    r_tid <- getRemoteThreadIdFromRemoteContext (GHC.resumeContext r)
    case (r_tid == req_tid, GHC.resumeBreakpointId r) of
      (True, Nothing) -> do
        let excRef = resumeApStack r
        fromMaybe defaultExceptionInfo <$> exceptionInfoFromContext (castForeignRef excRef)
      _ -> return defaultExceptionInfo

-- | Evaluate helper code inside the debuggee that turns the exception context
-- into our 'ExceptionInfo' structure.
exceptionInfoFromContext :: ForeignRef SomeException -> Debugger (Maybe ExceptionInfo)
exceptionInfoFromContext excRef = do
#if MIN_VERSION_ghc(9,15,0)
  Just <$> Debuggee.collectExceptionInfo excRef
#else
  Debuggee.collectExceptionInfo excRef
#endif

-- | Placeholder exception info returned when the context could not be
-- inspected.
defaultExceptionInfo :: ExceptionInfo
defaultExceptionInfo = ExceptionInfo
  { exceptionInfoTypeName = "Exception"
  , exceptionInfoFullTypeName = "Exception"
  , exceptionInfoMessage = "Exception information not available"
  , exceptionInfoContext = Nothing
  , exceptionInfoSourceSpan = Nothing
  , exceptionInfoInner = []
  }
