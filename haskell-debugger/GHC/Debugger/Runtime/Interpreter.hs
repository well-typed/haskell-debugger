{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | Custom interpreter commands in addition to the existing
-- 'GHC.Runtime.Interpreter' ones.
--
-- Similarly to 'GHC.Runtime.Interpreter', the functions in this module
-- abstract whether the external or internal interpreter is being used.
--
-- When using the external interpreter, we leverage the "Custom external commands"
-- feature introduced in GHC 10.0's ghci API to extend the external interpreter
-- server with custom command messages and handlers. When using
--
-- In GHC 9.14, there isn't support for extending the external interpreter server with new commands.
-- Thus, it is impossible to get the external interpreter to execute custom
-- code. When custom commands aren't available for the external interpreter we
-- fallback to an implementation which uses more primitive functions on the
-- external interpreter, decode the heap values directly into 'Term', and use
-- 'GHC.Debugger.Runtime.Term.Parser' to parse the heap 'Term's into structured
-- data.
--
-- When using the internal interpreter, we can execute the custom command
-- handler directly in this process -- regardless of whether using GHC 9.14 or GHC 10.0.
-- In practice, for simplicity, we just use the fallback mechanism of 9.14 for both internal and external.
--
-- The custom commands and their handlers are implemented in
-- 'GHC.Debugger.Runtime.Interpreter.Custom'
--
-- Meant to be imported with
--
-- @
-- import qualified GHC.Debugger.Runtime.Interpreter as Debuggee
-- @
--
-- since the exported
-- functions run on the debuggee (possibly external) process. Even on GHC 9.14,
-- where we use more primitive messages and do some extra parsing on the
-- debugger/host side
module GHC.Debugger.Runtime.Interpreter
  ( listThreads

  -- * Re-exports
  , ThreadInfo(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class

import GHCi.RemoteTypes
import GHC.Debugger.Monad

import GHC.Debugger.Runtime.Interpreter.Custom

#if MIN_VERSION_ghc(9,15,0)
import Data.Binary
import GHCi.Message
import GHC.Driver.Env (hscInterp)
import GHC.Driver.Monad (getSession)
import GHC.Runtime.Interpreter
#else
import Control.Applicative
import Control.Concurrent
import Data.Functor
import GHC.Conc.Sync (ThreadStatus(..), BlockReason(..))

import GHC.Builtin.Types
import GHC.Utils.Outputable

import Colog.Core as Logger

import GHC.Debugger.Runtime.Term.Parser
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin as Remote
#endif

--------------------------------------------------------------------------------
-- * Debugger abstraction
--
-- | The functions in this section need to be in 'Debugger' because the
-- 'TermParser' (needed to implement these top-level functions in GHC 9.14)
-- works in 'Debugger'. This also allows us to call debugger-interpreter
-- functions without 'liftIO' everywhere.
--------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,15,0)
--------------------------------------------------------------------------------
-- ** GHC 9.15/10.0 and above: use custom commands
--------------------------------------------------------------------------------

listThreads :: Debugger [ThreadInfo ForeignRef]
listThreads = do
  interp  <- hscInterp <$> getSession
  ts_info <- liftIO $ interpDbgCmd interp ListThreads
  forM ts_info $ \ti@ThreadInfo{threadInfoRef} -> do
    threadInfoForeignRef <- liftIO $ mkFinalizedHValue interp threadInfoRef
    pure ti{threadInfoRef = threadInfoForeignRef}

#else
--------------------------------------------------------------------------------
-- ** GHC 9.14: use @evalX@ and @TermParser@ to do it without custom commands
--------------------------------------------------------------------------------

listThreads :: Debugger [ThreadInfo ForeignRef]
listThreads = do
  threads_fvs <- expectRight =<< Remote.evalIOList Remote.listThreads
  labels      <- getRemoteThreadsLabels threads_fvs
  forM (zip threads_fvs labels) $ \(castForeignRef -> thread_fv, label) -> do
    status <- getRemoteThreadStatus thread_fv
    pure ThreadInfo
      { threadInfoStatus = status
      , threadInfoLabel  = label
      , threadInfoRef    = thread_fv
      }

-- | Is the remote thread running or blocked (NOT finished NOR dead)?
getRemoteThreadStatus :: ForeignRef ThreadId -> Debugger ThreadStatus
getRemoteThreadStatus threadIdRef = do
  status_fv  <- expectRight =<< Remote.evalIO
    (Remote.threadStatus (Remote.ref threadIdRef))
  status_parsed <-
    obtainParsedTerm "ThreadStatus" 2 True anyTy{-..no..-} (castForeignRef status_fv) threadStatusParser

  case status_parsed of
    Left errs -> do
      logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
      liftIO $ fail "Failed to parse ThreadStatus"
    Right thrdStatus ->
      return thrdStatus

getRemoteThreadsLabels :: [ForeignRef ThreadId] -> Debugger [Maybe String]
getRemoteThreadsLabels threadIdRefs = do

  forM threadIdRefs $ \threadIdRef -> do

    r <- Remote.evalIOList $ Remote.do
      mb_str <- Remote.threadLabel (Remote.ref threadIdRef)
      Remote.return (Remote.maybeToList mb_str)

    expectRight r >>= \case
      []          -> pure Nothing
      [io_lbl_fv] -> Just <$> (expectRight =<< Remote.evalString (Remote.ref io_lbl_fv))
      _ -> liftIO $ fail "Unexpected result from evaluating \"threadLabel\""

--------------------------------------------------------------------------------
-- *** TermParsers
--------------------------------------------------------------------------------

threadStatusParser :: TermParser ThreadStatus
threadStatusParser = do
        (matchConstructorTerm "ThreadRunning"  $> ThreadRunning)
    <|> (matchConstructorTerm "ThreadFinished" $> ThreadFinished)
    <|> (matchConstructorTerm "ThreadDied"     $> ThreadDied)
    <|> (matchConstructorTerm "ThreadBlocked"  *> (ThreadBlocked <$> subtermWith 0 blockedReasonParser))

blockedReasonParser :: TermParser BlockReason
blockedReasonParser = do
        (matchConstructorTerm "BlockedOnMVar"        $> BlockedOnMVar)
    <|> (matchConstructorTerm "BlockedOnBlackHole"   $> BlockedOnBlackHole)
    <|> (matchConstructorTerm "BlockedOnException"   $> BlockedOnException)
    <|> (matchConstructorTerm "BlockedOnSTM"         $> BlockedOnSTM)
    <|> (matchConstructorTerm "BlockedOnForeignCall" $> BlockedOnForeignCall)
    <|> (matchConstructorTerm "BlockedOnOther"       $> BlockedOnOther)


#endif

--------------------------------------------------------------------------------
-- * IO+interpreter abstraction
--
-- | Functions on IO which abstract calling the external interpreter or
-- internal interpreter using custom commands. Note that 'CustomMessage' is not
-- available in GHC 9.14 so we don't make these functions available in GHC 9.14
--------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,15,0)

-- | Run a 'DbgInterpCmd' in the interpreter's context. By default, the command is
-- serialized and sent to an external iserv process, and the response is
-- deserialized (hence the @Binary@ constraint). With @--internal-interpreter@
-- we execute the command directly here.
--
-- To run a builtin 'Message' command, use 'interpCmd' instead.
interpDbgCmd :: Binary a => Interp -> DbgInterpCmd a -> IO a
interpDbgCmd interp command = case interpInstance interp of
  InternalInterp ->
    -- Run it directly on this process!
    runDbgInterpCmd command
  ExternalInterp{}
    -- Use interpCmd to send the command as a custom message to external process.
    -- The custom message will be processed according to the custom command
    -- handlers registered with `iservWithCustom`
    | let payload = encodePayload (Some @Binary command) -> do
      respBytes <- interpCmd interp (CustomMessage dbgInterpCmdTag payload)
      case decodePayload respBytes of
        Left err -> fail err
        Right r  -> pure r

#endif
