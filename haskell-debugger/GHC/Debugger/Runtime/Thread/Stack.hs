{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | Decoding the stack of a thread at runtime
module GHC.Debugger.Runtime.Thread.Stack
  ( getRemoteThreadStackCopy
  , getRemoteThreadIPEStack
  ) where

import Data.Bits
import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import GHC.Stack.CloneStack
import GHC.Exts.Heap.ClosureTypes
import GHC.Utils.Encoding.UTF8

import GHC
import GHC.Builtin.Types
import GHC.Runtime.Heap.Inspect

import GHC.Driver.Env
import GHC.Runtime.Interpreter as Interp
import GHC.Utils.Outputable

import GHCi.Message
import GHCi.RemoteTypes

import GHC.Debugger.Utils
import GHC.Debugger.Logger as Logger
import GHC.Debugger.Monad
import GHC.Debugger.Runtime.Term.Parser
import GHC.Debugger.Runtime.Eval
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin as Remote

-- | Clone the stack of the given remote thread and get the breakpoint ids of available frames
getRemoteThreadStackCopy :: ForeignRef ThreadId -> Debugger [InternalBreakpointId]
getRemoteThreadStackCopy threadIdRef = do

  l <- Remote.evalIOList $ Remote.do
    clonedStack <- Remote.cloneThreadStack (Remote.ref threadIdRef)
    frames      <- Remote.decodeStack      clonedStack
    Remote.return (Remote.ssc_stack frames)

  case l of
    Left (EvalRaisedException e) -> do
      logSDoc Logger.Info (text "Failed to decode the stack with" <+> text (show e) $$ text "This is likely bug #26640 in the decoder, which has been fixed for 9.14.2 and forward. No StackTrace will be returned...")
      return []
    Left e -> do
      logSDoc Logger.Warning (text "Failed to decode the stack with" <+> text (show e) $$ text "No StackTrace will be returned...")
      return []
    Right stack_frames_fvs -> fmap (catMaybes . catMaybes) $
      forM stack_frames_fvs $ \stack_frame_fv ->
        obtainParsedTerm "ghc-heap:StackFrame" 2 True anyTy{-todo:stackframety?-} (castForeignRef stack_frame_fv)
          ((Just <$> retBCOParser) <|> pure Nothing) >>= \case
            Left errs -> do
              logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
              return Nothing
            Right tm ->
              return tm

-- | Try to get an IPE stacktrace.
--
-- At the moment, we assume IPE stacktraces are always empty @[]@ for threads
-- with interpreter frames.
--
-- Really, as long as there is IPE information, this function should return
-- StackEntries for all frames, including the interpreter ones, since these are
-- typically be interleaved with "normal" frames.
--
getRemoteThreadIPEStack :: ForeignRef ThreadId -> Debugger [StackEntry]
getRemoteThreadIPEStack threadIdRef = do
  l <- Remote.evalIOList $ Remote.do
    clonedStack <- Remote.cloneThreadStack (Remote.ref threadIdRef)
    Remote.decode clonedStack
  case l of
    Left (EvalRaisedException e) -> do
      logSDoc Logger.Info (text "Failed to decode the stack with" <+> text (show e) $$ text "This is likely bug #26640 in the decoder, which has been fixed for 9.14.2 and forward. No StackTrace could be produced...")
      return []
    Left e -> do
      logSDoc Logger.Warning (text "Failed to decode the stack with" <+> text (show e) $$ text "No StackTrace will be returned...")
      return []
    Right entries_fvs -> do
      mapM (\entry_fv -> do
          stack_entry <- obtainParsedTerm "StackEntry" 2 True anyTy{-stackentry, but we won't look at the ty...-} entry_fv stackEntryParser
          case stack_entry of
            Left errs -> do
              logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
              liftIO $ fail "Failed to parse @StackEntry@ from decoding thread stack!"
            Right se ->
              pure se
        ) (map castForeignRef entries_fvs)

--------------------------------------------------------------------------------
-- ** Decoding Stack Frames ----------------------------------------------------
--------------------------------------------------------------------------------

stackEntryParser :: TermParser StackEntry
stackEntryParser = do
    StackEntry <$> subtermWith 0 stringParser <*> subtermWith 1 stringParser <*> subtermWith 2 stringParser <*> pure INVALID_OBJECT{-this is a stub-}

retBCOParser :: TermParser (Maybe InternalBreakpointId)
retBCOParser = do
  -- Match against "RetBCO" frames and extract the BCOClosure information
  Suspension{val, ctype=BCO}
    {-"the otherwise case: Unknown closure", hence Suspension-}
      <- matchConstructorTerm "RetBCO" *> subtermWith 1 (subtermWith 0{-take from Box-} anyTerm)
  liftDebugger $ do

    -- Decode the BCO closure using 'getClosureData' on the foreign heap
    bco_closure_fv <- expectRight =<< Remote.evalIO
      (Remote.getClosureData (Remote.ref (castForeignRef val)))

    r <- obtainParsedTerm "BCO BRK_FUN info" 2 True anyTy (castForeignRef bco_closure_fv) bcoInternalBreakpointId
    case r of
      Left err -> liftIO $ fail (show err)
      Right t  -> return t

-- | Parse an 'InternalBreakpointId' out of a 'BCOClosure' term.
bcoInternalBreakpointId :: TermParser (Maybe InternalBreakpointId)
bcoInternalBreakpointId = do
  mbcpIxs <- bcoBreakPointInfoParser
  case mbcpIxs of
    Nothing -> return Nothing
    Just BCOBreakPointInfo{..} -> do
      mod_name <- bcoLiteralString info_mod_name_ix
      mod_id   <- bcoLiteralString info_mod_id_ix

      return $ Just $ evalBreakpointToId EvalBreakpoint
        { eb_info_mod      = mod_name
        , eb_info_mod_unit = utf8EncodeShortByteString mod_id
        , eb_info_index    = fromIntegral $ brk_info_ix_hi .<<. 16 + brk_info_ix_lo
        }

-- | Parse a literal 'String' from a BCO given a valid index into the literals array
bcoLiteralString :: Word -> TermParser String
bcoLiteralString ix = do
  Term{val=literals_fv} <- subtermWith 2 (subtermTerm 0{-Box's field-})
  liftDebugger $ do

    r <- Remote.evalIOString $
        Remote.peekCString $
          Remote.withUnboxed (Remote.lit (fromIntegral ix))
            (Remote.indexAddrArray (Remote.untypedRef literals_fv))

    expectRight r

-- | The indexes found in the BRK_FUN instruction
data BCOBreakPointInfo = BCOBreakPointInfo
  { brk_array_ix     :: !Word
  , info_mod_name_ix :: !Word
  , info_mod_id_ix   :: !Word
  , brk_info_ix_hi   :: !Word
  , brk_info_ix_lo   :: !Word
  }
  deriving Show

-- | Parses a 'BCOBreakPoint' if the current term is a 'BCOClosure' headed by a
-- BRK_FUN bytecode instruction.
-- Returns Nothing if the 'BCOClosure' instructions are headed by a BRK_FUN.
bcoBreakPointInfoParser :: TermParser (Maybe BCOBreakPointInfo)
bcoBreakPointInfoParser = do
  Term{val=instrs_array_fv} <- subtermWith 1{-instrs field-} (subtermTerm 0{-Box's field-})
  -- highly internals dependent...
  -- find the BCI at index 0. bci is word16. the first 8bits are for flags
  -- something something BCO_READ_LARGE_ARG with (index_at 0#) rather than always BCO_NEXT?
  liftDebugger $ do
    hsc_env <- getSession

    -- The BRK_FUN is the first instruction, unless BCO_NAME is enabled, in
    -- which case it's the second.
    let bRK_FUN_offset
          | gopt Opt_AddBcoName (hsc_dflags hsc_env) = 2 -- BCO_NAME + ptrs ix.
          | otherwise = 0 :: Int

    let find_ixs_fv = Remote.raw $
          "\\x -> let index_at n = GHC.Word.W16# (GHC.Base.indexWord16Array# x (n GHC.Exts.+# " ++ show bRK_FUN_offset ++ "#)) " ++
                   "in if (index_at 0# Data.Bits..&. 0xFF) == 66{-bci_BRK_FUN-} then \
                        Just (index_at 1#, index_at 2#, index_at 3#, index_at 4#, index_at 5#) \
                      else Nothing"
    rs_fv <- expectRight =<< Remote.eval
      (find_ixs_fv `Remote.app` Remote.untypedRef instrs_array_fv)

    mparsed_bco_brk <- obtainParsedTerm "Ixs" maxBound True anyTy rs_fv $
      maybeParser $ BCOBreakPointInfo <$>
        subtermWith 0 wordParser <*> subtermWith 1 wordParser <*> subtermWith 2 wordParser
                                 <*> subtermWith 3 wordParser <*> subtermWith 4 wordParser
    case mparsed_bco_brk of
      Left errs -> do
        logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
        liftIO $ fail "Failed to parse BCOClosure's BRK_FUN"
      Right r -> return r
