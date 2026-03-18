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
-- data. That implementation doesn't live in this module, which is meant only
-- to be the interface to the custom commands supported by GHC > 9.14.
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

import Data.Binary
import GHCi.Message
import GHC.Driver.Env (hscInterp)
import GHC.Driver.Monad (getSession)
import GHC.Runtime.Interpreter

--------------------------------------------------------------------------------
-- * Debugger abstraction
--
-- | The functions in this section need to be in 'Debugger' because the
-- 'TermParser' (needed to implement these top-level functions in GHC 9.14)
-- works in 'Debugger'. This also allows us to call debugger-interpreter
-- functions without 'liftIO' everywhere.
--------------------------------------------------------------------------------

listThreads :: Debugger [ThreadInfo ForeignRef]
listThreads = do
  interp  <- hscInterp <$> getSession
  ts_info <- liftIO $ interpDbgCmd interp ListThreads
  forM ts_info $ \ti@ThreadInfo{threadInfoRef} -> do
    threadInfoForeignRef <- liftIO $ mkFinalizedHValue interp threadInfoRef
    pure ti{threadInfoRef = threadInfoForeignRef}

--------------------------------------------------------------------------------
-- * IO+interpreter abstraction
--
-- | Functions on IO which abstract calling the external interpreter or
-- internal interpreter using custom commands. Note that 'CustomMessage' is not
-- available in GHC 9.14 so we don't make these functions available in GHC 9.14
--------------------------------------------------------------------------------

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
