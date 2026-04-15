{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- necessary Binary instances
module GHC.Debugger.Runtime.Interpreter.Custom where

import GHC.Generics (Generic)

import GHCi.Message
import GHCi.RemoteTypes

import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import Foreign.C.String
import GHC.ByteCode.Types
import GHC.Conc.Sync
import GHC.Exts.Heap.Closures (StackFrame, GenStackFrame (..), Box (..))
import GHC.InfoProv
import GHC.Runtime.Interpreter (evalBreakpointToId)
import GHC.Stack.Annotation.Experimental
import GHC.Utils.Encoding.UTF8 (utf8EncodeShortByteString)
import GHC.Word
import Unsafe.Coerce (unsafeCoerce, unsafeCoerce#)
import qualified GHC.Base
import qualified GHC.Exts
import qualified GHC.Exts.Heap as Heap
import qualified GHC.Stack as Stack
import GHC.Unit.Module
import Control.Exception
import GHC.Debugger.Interface.Messages (SourceSpan (..), ExceptionInfo (..))
import Control.Exception.Context
import Data.Typeable
#if MIN_VERSION_ghc(9,15,0)
import GHC.Debugger.Interface.Messages (srcLocToSourceSpan)
import Data.Maybe
import Control.Exception.Backtrace
import GHC.Exception
import qualified GHC.Stack.CloneStack as Stack
import qualified GHC.Stack.Decode.Experimental as Stack
import qualified GHC.Exception.Backtrace.Experimental as Backtrace
#endif

--------------------------------------------------------------------------------
-- * Custom Commands
--------------------------------------------------------------------------------

data ThreadInfo ref = ThreadInfo
  { threadInfoRef    :: !(ref ThreadId)
  , threadInfoLabel  :: !(Maybe String)
  , threadInfoStatus :: !ThreadStatus
  }
  deriving (Generic)

-- | Information about a stack frame
data StackFrameInfo
  -- | Information derived from an IPE entry
  = StackFrameIPEInfo !InfoProv
  -- | User-defined Stack Frame annotation
  | StackFrameAnnotation !(Maybe Stack.SrcLoc) !String
  -- | Information derived from a continuation BCO breakpoint info.
  | StackFrameBreakpointInfo !InternalBreakpointId
  deriving (Generic)

data DbgInterpCmd a where
  ListThreads :: DbgInterpCmd [ThreadInfo RemoteRef]
  DecodeThreadStack :: RemoteRef ThreadId -> DbgInterpCmd [StackFrameInfo]
  CollectExceptionInfo :: RemoteRef SomeException -> DbgInterpCmd ExceptionInfo

dbgInterpCmdTag :: Word8
dbgInterpCmdTag = 0x25

--------------------------------------------------------------------------------
-- * Running and handlers
--------------------------------------------------------------------------------

-- | Run a custom 'DbgInterpCmd' directly.
runDbgInterpCmd :: DbgInterpCmd a -> IO a
runDbgInterpCmd = \case
  ListThreads -> mapM threadInfo =<< listThreads
#if MIN_VERSION_ghc(9,14,2)
  -- decodeStackWithIpe exposed from 9.14.2 onwards (see #27065)
  DecodeThreadStack threadIdRef -> do
    -- We clone the stack of the thread and decode it in the external interpreter to avoid
    -- any potential issues with decoding the stack in the main interpreter, which can be
    -- more fragile due to optimizations and other factors. This also allows us to leverage
    -- the existing machinery for decoding stacks in the external interpreter, which is more
    -- robust and can handle a wider variety of cases.
    threadId    <- localRef threadIdRef
    clonedStack <- Stack.cloneThreadStack threadId
    frames      <- Stack.decodeStackWithIpe clonedStack
    catMaybes <$> mapM stackFrameInfo frames
#else
  DecodeThreadStack _ -> fail "Decoding thread stacks is not supported on GHC versions prior to 9.14.2, which exposes the necessary decodeStackWithIpe functions. Please upgrade to GHC 9.14.2 or later to use this feature."
#endif
  CollectExceptionInfo excRef -> do
    exc  <- localRef excRef
    let info = exceptionInfo exc
    return info


-- | Run a serialized custom 'DbgInterpCmd'. This is used in conjunction with
-- 'servWithCustom' to handle custom messages sent to the external interpreter.
dbgInterpCmdHandler :: Word8 -> BS.ByteString -> IO (Maybe BS.ByteString)
dbgInterpCmdHandler tag payload
  | tag == dbgInterpCmdTag =
      case decodePayload payload of
        Left err ->
          fail ("Failed to decode debugger thread command payload: " ++ err)
        Right (Some @Bin.Binary command) ->
          Just . encodePayload <$> runDbgInterpCmd command
  | otherwise =
      pure Nothing

--------------------------------------------------------------------------------
-- * Implementing each command
--------------------------------------------------------------------------------

-- | Read some info about a Thread from its 'ThreadId'
threadInfo :: ThreadId -> IO (ThreadInfo RemoteRef)
threadInfo threadId = do
  status <- threadStatus threadId
  ref    <- mkRemoteRef threadId
  label  <- threadLabel threadId
  pure ThreadInfo
    { threadInfoRef    = ref
    , threadInfoLabel  = label
    , threadInfoStatus = status
    }

-- | Try to make a 'StackFrameInfo' out of the decoded stack frame information:
--
-- 1. Try stack annotations first
-- 2. Try IPE next
-- 3. Try decoding a continuation BCO with a breakpoint next
stackFrameInfo :: (StackFrame, Maybe InfoProv) -> IO (Maybe StackFrameInfo)
stackFrameInfo (AnnFrame{annotation}, _)
  | let Box annVal = annotation
  , let stack_anno = displayStackAnnotation @SomeStackAnnotation (unsafeCoerce annVal)
  = pure $ Just $ StackFrameAnnotation Nothing{-No source locations yet-} stack_anno
stackFrameInfo (_, Just ipe)
  = pure $ Just (StackFrameIPEInfo ipe)
stackFrameInfo (RetBCO{bco}, _)
  | let !(Box !bco_hval) = bco -- needs to be forced for `getClosureData` to look at the right thing.
  = fmap StackFrameBreakpointInfo <$> (lookupBCOBreakpoint =<< Heap.getClosureData bco_hval)
stackFrameInfo _
  = pure Nothing

-- | Try to find a BRK_FUN breakpoint location at the start of a BCO heap closure
lookupBCOBreakpoint :: Heap.GenClosure Box -> IO (Maybe InternalBreakpointId)
lookupBCOBreakpoint Heap.BCOClosure{..}
  {-
  This is highly internals dependent...:
    - find the bytecode instruction (BCI) at index 0
    - BCI is Word16 (the high 8bits are for flags, low for instruction op code)

  NOTE: Assume @Opt_AddBcoName@ is not set. @Opt_AddBcoName@ moves the BRK_FUN
  BCI to index 2#, but it is a debug flag used by compiler developers to debug
  bytecode itself, so we shouldn't go to great lengths to support it. Other
  things will be broken too.
  -}
  | index_at 0# .&. 0xFF == 66{-bci_BRK_FUN-}
  = do
    mod_name <- literalString info_mod_name_ix
    mod_id   <- literalString info_mod_id_ix
    pure $ Just $ evalBreakpointToId EvalBreakpoint
#if MIN_VERSION_ghc(9,15,0)
      { eb_info_mod      = utf8EncodeShortByteString mod_name
#else
      { eb_info_mod      = mod_name
#endif
      , eb_info_mod_unit = utf8EncodeShortByteString mod_id
      , eb_info_index    = fromIntegral $ brk_info_ix_hi .<<. 16 + brk_info_ix_lo
      }

  | otherwise
  = pure Nothing

  where
    Box instrs_val = instrs
    Box literals_val = literals

    index_at n = W16# (GHC.Base.indexWord16Array# (unsafeCoerce# instrs_val) n)

    -- find a string in the literals array by index
    literalString :: Word16 -> IO String
    literalString (fromIntegral -> GHC.Exts.I# ix) = do
      peekCString $
        GHC.Exts.Ptr (GHC.Exts.indexAddrArray# (unsafeCoerce# literals_val) ix)

    _brk_arr_ix      = index_at 1#
    info_mod_name_ix = index_at 2#
    info_mod_id_ix   = index_at 3#
    brk_info_ix_hi   = index_at 4#
    brk_info_ix_lo   = index_at 5#
lookupBCOBreakpoint _ = pure Nothing

exceptionInfo :: SomeException -> ExceptionInfo
exceptionInfo se'@(SomeException exc) =
    ExceptionInfo
       { exceptionInfoTypeName = simpleTypeName
       , exceptionInfoFullTypeName = fullTypeName
       , exceptionInfoMessage = Control.Exception.displayException se'
       , exceptionInfoContext = contextText
       , exceptionInfoSourceSpan = exceptionContextLocation
       , exceptionInfoInner = innerNodes
       }
  where
    ctx = someExceptionContext se'
    rendered = displayExceptionContext ctx
    whileHandling = getExceptionAnnotations ctx
    innerNodes = map (exceptionInfo . unwrap) whileHandling
    simpleTypeName = tyConName tc
    modulePrefix = case tyConModule tc of
      mdl | null mdl -> ""
          | otherwise -> mdl ++ "."
    packagePrefix = case tyConPackage tc of
      pkg | null pkg -> ""
          | otherwise -> pkg ++ ":"
    tc = typeRepTyCon (typeOf exc)
    fullTypeName = packagePrefix ++ modulePrefix ++ simpleTypeName
    unwrap (WhileHandling inner) = inner
    contextText | null rendered = Nothing
                | otherwise     = Just rendered

#if MIN_VERSION_ghc(9,15,0)
    exceptionContextLocation =
      let fromCallStack cs = case listToMaybe (getCallStack cs) of
            Just (_, loc) -> Just (srcLocToSourceSpan loc)
            Nothing       -> Nothing
          bts :: [Backtraces]
          bts = getExceptionAnnotations ctx
      in case bts of
           bt : _ -> case Backtrace.btrHasCallStack bt of
             Just cs -> fromCallStack cs
             Nothing -> Nothing
           [] -> Nothing
#else
    exceptionContextLocation = Nothing {- btrHasCallstack not available -}
#endif

--------------------------------------------------------------------------------
-- * Binary for custom commands
--------------------------------------------------------------------------------

data Some c f where
  Some :: c a => f a -> Some c f

encodePayload :: Bin.Binary a => a -> BS.ByteString
encodePayload = BL.toStrict . Bin.encode

decodePayload :: Bin.Binary a => BS.ByteString -> Either String a
decodePayload bs =
  case Bin.decodeOrFail (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, a)  -> Right a

instance Bin.Binary (Some Bin.Binary DbgInterpCmd) where
  put (Some command) = case command of
    ListThreads -> Bin.put (0 :: Word8)
    DecodeThreadStack threadIdRef -> do
      Bin.put (1 :: Word8)
      Bin.put threadIdRef
    CollectExceptionInfo excRef -> do
      Bin.put (2 :: Word8)
      Bin.put excRef

  get = do
    (tag :: Word8) <- Bin.get
    case tag of
      0 -> pure (Some ListThreads)
      1 -> Some . DecodeThreadStack <$> Bin.get
      2 -> Some . CollectExceptionInfo <$> Bin.get
      _ -> fail ("Unknown debugger thread command tag: " ++ show tag)

instance Bin.Binary (ThreadInfo RemoteRef)
instance Bin.Binary StackFrameInfo
instance Bin.Binary Stack.SrcLoc
instance Bin.Binary SourceSpan where
  put SourceSpan{..} = do
    Bin.put file
    Bin.put startLine
    Bin.put endLine
    Bin.put startCol
    Bin.put endCol

  get = do
    file <- Bin.get
    startLine <- Bin.get
    endLine <- Bin.get
    startCol <- Bin.get
    endCol <- Bin.get
    pure SourceSpan{..}

instance Bin.Binary ExceptionInfo where
  put ExceptionInfo{..} = do
    Bin.put exceptionInfoTypeName
    Bin.put exceptionInfoFullTypeName
    Bin.put exceptionInfoMessage
    Bin.put exceptionInfoContext
    Bin.put exceptionInfoSourceSpan
    Bin.put exceptionInfoInner

  get = do
    exceptionInfoTypeName <- Bin.get
    exceptionInfoFullTypeName <- Bin.get
    exceptionInfoMessage <- Bin.get
    exceptionInfoContext <- Bin.get
    exceptionInfoSourceSpan <- Bin.get
    exceptionInfoInner <- Bin.get
    pure ExceptionInfo{..}

instance Bin.Binary InternalBreakpointId where
  put (InternalBreakpointId (Module (RealUnit (Definite unit)) name) ix) = do
    Bin.put (unitString unit)
    Bin.put (moduleNameString name)
    Bin.put ix
  put _ = error "indefinite units are not supported by haskell-debugger yet!"
  get = do
    unit <- stringToUnitId <$> Bin.get
    name <- mkModuleName <$> Bin.get
    ix   <- Bin.get
    pure $ InternalBreakpointId (Module (RealUnit (Definite unit)) name) ix

instance Bin.Binary ThreadStatus where
  put = \case
    ThreadRunning -> Bin.put (0 :: Word8)
    ThreadFinished -> Bin.put (1 :: Word8)
    ThreadBlocked blockReason -> do
      Bin.put (2 :: Word8)
      Bin.put blockReason
    ThreadDied -> Bin.put (3 :: Word8)

  get = do
    (statusTag :: Word8) <- Bin.get
    case statusTag of
      0 -> pure ThreadRunning
      1 -> pure ThreadFinished
      2 -> ThreadBlocked <$> Bin.get
      3 -> pure ThreadDied
      _ -> fail ("Unknown thread status tag: " ++ show statusTag)

instance Bin.Binary BlockReason where
  put = \case
    BlockedOnMVar        -> Bin.put (0 :: Word8)
    BlockedOnBlackHole   -> Bin.put (1 :: Word8)
    BlockedOnException   -> Bin.put (2 :: Word8)
    BlockedOnSTM         -> Bin.put (3 :: Word8)
    BlockedOnForeignCall -> Bin.put (4 :: Word8)
    BlockedOnOther       -> Bin.put (5 :: Word8)

  get = do
    blockTag <- Bin.get
    case (blockTag :: Word8) of
      0 -> pure BlockedOnMVar
      1 -> pure BlockedOnBlackHole
      2 -> pure BlockedOnException
      3 -> pure BlockedOnSTM
      4 -> pure BlockedOnForeignCall
      5 -> pure BlockedOnOther
      _ -> fail ("Unknown thread block reason tag: " ++ show blockTag)

--------------------------------------------------------------------------------
