{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module GHC.Debugger.Runtime.Interpreter.Legacy
  ( listThreads
  , decodeThreadStack
  , collectExceptionInfo
  ) where

import Control.Exception (SomeException)
import System.Directory (getCurrentDirectory)
import Colog.Core as Logger
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.Functor
import Data.Maybe
import GHC
import GHC.Builtin.Types
import GHC.Conc.Sync hiding (listThreads)
import GHC.Debugger.Interface.Messages
import GHC.Debugger.Monad
import GHC.Debugger.Runtime.Eval
import GHC.Debugger.Runtime.Interpreter.Types
import GHC.Debugger.Runtime.Term.Parser
import GHC.Driver.Env
import GHC.Exts.Heap.ClosureTypes
import GHC.InfoProv
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import GHC.Utils.Encoding.UTF8
import GHC.Utils.Outputable as Ppr
import GHCi.Message
import GHCi.RemoteTypes
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr.Builtin as Remote
import qualified GHC.Stack.Types as Stack

-- GHC 9.14: use @evalX@ and @TermParser@ to do this all without custom commands

--------------------------------------------------------------------------------
-- * Threads
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


--------------------------------------------------------------------------------
-- * Thread stack frames
--------------------------------------------------------------------------------

decodeThreadStack :: ForeignRef ThreadId -> Debugger [StackFrameInfo]
decodeThreadStack threadIdRef = do
  l <- Remote.evalIOList $ Remote.do
    clonedStack <- Remote.cloneThreadStack (Remote.ref threadIdRef)
    frames      <- Remote.decodeStackWithIpe clonedStack
    Remote.return frames

  case l of
    Left (EvalRaisedException e) -> do
      logSDoc Logger.Info (text "Failed to decode the stack with" <+> text (show e) $$ text "This is likely bug #26640 in the decoder, which has been fixed for 9.14.2 and forward. No StackTrace will be returned...")
      return []
    Left e -> do
      logSDoc Logger.Warning (text "Failed to decode the stack with" <+> text (show e) $$ text "No StackTrace will be returned...")
      return []
    Right stack_frames_fvs -> fmap catMaybes $
      forM stack_frames_fvs $ \ stack_frame_fv -> do
        obtainParsedTerm "ghc-heap:StackFrame" 2 True anyTy{-todo:stackframety?-} (castForeignRef stack_frame_fv)
          stackFrameInfoParser >>= \case
            Left errs -> do
              logSDoc Logger.Error (vcat (map (text . getTermErrorMessage) errs))
              return Nothing
            Right tm ->
              return tm

--------------------------------------------------------------------------------
-- ** Decoding Stack Frames ----------------------------------------------------
--------------------------------------------------------------------------------

-- | Try to decode a 'StackFrameInfo' from a @(StackFrame, Maybe InfoProv)@ term
stackFrameInfoParser :: TermParser (Maybe StackFrameInfo)
stackFrameInfoParser = do
  -- Try a stack annotation first
  stackAnno <- subtermWith 0 stackAnnoParser
  case stackAnno of
    Nothing -> do
      -- Try IPE next
      mipe <- subtermWith 1 (maybeParser infoProvParser)
      case mipe of
        Nothing -> do
          -- Try decoding a continuation BCO with a breakpoint next
          fmap StackFrameBreakpointInfo
            <$> subtermWith 0 retBCOParser
        Just ipe -> pure $
          Just (StackFrameIPEInfo ipe)
    Just (srcLoc, ann) -> pure $
      Just (StackFrameAnnotation srcLoc ann)

-- | Decode an 'InfoProv' from an @InfoProv@ term
infoProvParser :: TermParser InfoProv
infoProvParser = InfoProv
  <$> subtermWith 0 stringParser -- ipName
  <*> pure INVALID_OBJECT -- ipDesc (this is a stub)
  <*> subtermWith 2 stringParser -- ipTyDesc
  <*> subtermWith 3 stringParser -- ipLabel
  <*> subtermWith 4 stringParser -- ipUnitId
  <*> subtermWith 5 stringParser -- ipMod
  <*> subtermWith 6 stringParser -- ipSrcFile
  <*> subtermWith 7 stringParser -- ipSrcSpan

-- | Try to decode an 'InternalBreakpointId' from a @StackFrame@ term
retBCOParser :: TermParser (Maybe InternalBreakpointId)
retBCOParser = do
  -- Match against "RetBCO" frames and extract the BCOClosure information
  (matchConstructorTerm "RetBCO" *> subtermWith 1 (subtermWith 0{-take from Box-} (Just <$> anyTerm)) <|> pure Nothing)
    >>= \case
      Just Suspension{val, ctype=BCO} -> do
        {-"the otherwise case: Unknown closure", hence Suspension-}

        -- Decode the BCO closure using 'getClosureData' on the foreign heap
        bco_closure_fv <- liftDebugger $
          expectRight =<< Remote.evalIO
            (Remote.getClosureData (Remote.ref (castForeignRef val)))

        r <- liftDebugger $
          obtainParsedTerm "BCO BRK_FUN info" 2 True anyTy (castForeignRef bco_closure_fv) bcoInternalBreakpointId
        case r of
          Left err -> fail (show err)
          Right t  -> return t
      _ -> pure Nothing

-- | Try to decode an 'StackAnnotation' from a @StackFrame@ term
stackAnnoParser :: TermParser (Maybe (Maybe Stack.SrcLoc, String))
stackAnnoParser = do
  -- Match against "AnnFrame" frames and extract the 'SomeStackAnnotation'
  (matchConstructorTerm "AnnFrame" *> subtermWith 1 (subtermWith 0{-take from Box-} (Just <$> anyTerm)) <|> pure Nothing)
    >>= \case
      Just Term{val} -> do
        stack_anno <- liftDebugger $
          expectRight =<< Remote.evalString
#if MIN_VERSION_ghc_experimental(9,1402,0)
            (Remote.displayStackAnnotationShort (Remote.ref (castForeignRef val)))
#else
            (Remote.displayStackAnnotation (Remote.ref (castForeignRef val)))
#endif

        src_loc <- getOptionalStackAnnotationSrcLoc

        pure $ Just (src_loc, stack_anno)
      _ ->
        pure Nothing

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

getOptionalStackAnnotationSrcLoc :: TermParser (Maybe Stack.SrcLoc)
#if MIN_VERSION_ghc_experimental(9,1402,0)
getOptionalStackAnnotationSrcLoc = do
  src_loc_fv <- liftDebugger $
    expectRight =<< Remote.eval
      (Remote.stackAnnotationSourceLocation (Remote.ref (castForeignRef val)))

  src_loc_either <- liftDebugger $
    obtainParsedTerm "Annotation SrcLoc" maxBound True anyTy (castForeignRef src_loc_fv) (maybeParser srcLocParser)

  case src_loc_either of
    Left err -> fail (show err)
    Right t  -> return t
 where
  -- | Parse a 'SrcLoc'.
  srcLocParser :: TermParser Stack.SrcLoc
  srcLocParser = do
    Stack.SrcLoc
      <$> subtermWith 0 stringParser -- srcLocPackage
      <*> subtermWith 1 stringParser -- srcLocModule
      <*> subtermWith 2 stringParser -- srcLocFile
      <*> subtermWith 3 intPrimParser -- unpacked srcLocStartLine
      <*> subtermWith 4 intPrimParser -- unpacked srcLocStartCol
      <*> subtermWith 5 intPrimParser -- unpacked srcLocEndLine
      <*> subtermWith 6 intPrimParser -- unpacked srcLocEndCol
#else
getOptionalStackAnnotationSrcLoc = do
  pure Nothing
#endif


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
          "\\x -> let index_at n = GHC.Word.W16# (GHC.Base.indexWord16Array# x (n GHC.Exts.+# " ++ show bRK_FUN_offset ++ """#))
                    in if (index_at 0# Data.Bits..&. 0xFF) == 66{-bci_BRK_FUN-} then
                        Data.Maybe.Just (index_at 1#, index_at 2#, index_at 3#, index_at 4#, index_at 5#)
                      else Data.Maybe.Nothing"""
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

--------------------------------------------------------------------------------
-- * Exception Info
--------------------------------------------------------------------------------

-- | Evaluate helper code inside the debuggee that turns the exception context
-- into our 'ExceptionInfo' structure.
collectExceptionInfo :: ForeignRef SomeException -> Debugger (Maybe ExceptionInfo)
collectExceptionInfo excRef = do
  -- 1. Add a "data" declaration for the datatype the expression will return
  _ <- runDecls exceptionInfoData
  -- 2. Gather information about the exception.
  evalRes <- Remote.eval
    (Remote.raw exceptionInfoExpr `Remote.app` Remote.ref excRef)
  case evalRes of
    Left err -> do
      logSDoc Logger.Debug $
        Ppr.text "Failed to evaluate exception info:" Ppr.<+> Ppr.text (show err)
      return Nothing
    Right fhv -> do
      parsed <- obtainParsedTerm "Exception info" 4 True anyTy (castForeignRef fhv)
        exceptionInfoParser
      case parsed of
        Left errs -> do
          logSDoc Logger.Debug $
            Ppr.text "Failed to parse exception info:"
              Ppr.<+> Ppr.vcat (map (Ppr.text . getTermErrorMessage) errs)
          return Nothing
        Right info -> return (Just info)

-- | Parse the helper 'ExceptionInfoNode' structure produced inside the
-- debuggee into our externally facing 'ExceptionInfo'.
exceptionInfoParser :: TermParser ExceptionInfo
exceptionInfoParser = do
  cwd <- liftIO $ mkAbsolute <$> getCurrentDirectory
  ExceptionInfo
    <$> subtermWith 0 stringParser
    <*> subtermWith 1 stringParser
    <*> subtermWith 2 stringParser
    <*> subtermWith 3 (maybeParser stringParser)
    <*> subtermWith 4 (maybeParser $ exceptionLocationTupleParser cwd)
    <*> subtermWith 5 (parseList exceptionInfoParser)
  where
    -- Parsed from @(String, Int, Int)@.
    -- See Note [Paths should be made absolute at the source]
    exceptionLocationTupleParser :: AbsFilePath -> TermParser SourceSpan
    exceptionLocationTupleParser prefix = do
      locFile <- subtermWith 0 stringParser
      srcLine <- subtermWith 1 intParser
      srcCol <- subtermWith 2 intParser
      pure SourceSpan
        { file = prefix /> locFile
        , startLine = srcLine
        , startCol = srcCol
        , endLine = srcLine
        , endCol = srcCol
        }

-- | Definition for the helper 'ExceptionInfoNode' data type compiled into the
-- debuggee to aid in transporting nested exception information.
-- We need a specific datatype because ExceptionInfoNode is recursive.
exceptionInfoData :: String
exceptionInfoData = """
  data ExceptionInfoNode = ExceptionInfoNode
    { exceptionNodeTypeName :: String
    , exceptionNodeFullTypeName :: String
    , exceptionNodeMessage :: String
    , exceptionNodeContext :: Data.Maybe.Maybe String
    , exceptionNodeSourceSpan :: Data.Maybe.Maybe (String, Int, Int)
    , exceptionNodeInner :: [ExceptionInfoNode]
    }
  """

-- | Helper expression run in the debuggee that walks the exception context and
-- populates the 'ExceptionInfoNode' structure.
exceptionInfoExpr :: String
exceptionInfoExpr = """
  let collectExceptionInfo :: Control.Exception.SomeException -> ExceptionInfoNode
      collectExceptionInfo se' =
        case se' of
          Control.Exception.SomeException exc ->
            let ctx = Control.Exception.someExceptionContext se'
                rendered = Control.Exception.Context.displayExceptionContext ctx
                whileHandling = Control.Exception.Context.getExceptionAnnotations ctx
                innerNodes = Prelude.map (collectExceptionInfo Prelude.. unwrap) whileHandling
                sourceSpan = exceptionContextLocation ctx
                simpleTypeName = Data.Typeable.tyConName tc
                modulePrefix = case Data.Typeable.tyConModule tc of
                  mdl | Prelude.null mdl -> \"\"
                      | otherwise -> mdl Prelude.++ \".\"
                packagePrefix = case Data.Typeable.tyConPackage tc of
                  pkg | Prelude.null pkg -> \"\"
                      | otherwise -> pkg Prelude.++ \":\"
                tc = Data.Typeable.typeRepTyCon (Data.Typeable.typeOf exc)
                fullTypeName = packagePrefix Prelude.++ modulePrefix Prelude.++ simpleTypeName
                unwrap (Control.Exception.WhileHandling inner) = inner
                contextText = if Prelude.null rendered then Data.Maybe.Nothing else Data.Maybe.Just rendered
            in ExceptionInfoNode
                 { exceptionNodeTypeName = simpleTypeName
                 , exceptionNodeFullTypeName = fullTypeName
                 , exceptionNodeMessage = Control.Exception.displayException se'
                 , exceptionNodeContext = contextText
                 , exceptionNodeSourceSpan = sourceSpan
                 , exceptionNodeInner = innerNodes
                 }
      exceptionContextLocation ctx =
        let fromCallStack cs = case Data.Maybe.listToMaybe (GHC.Exception.getCallStack cs) of
              Data.Maybe.Just (_, loc) ->
                Data.Maybe.Just
                  ( GHC.Exception.srcLocFile loc
                  , GHC.Exception.srcLocStartLine loc
                  , GHC.Exception.srcLocStartCol loc
                  )
              Data.Maybe.Nothing -> Data.Maybe.Nothing
            bts :: [Control.Exception.Backtrace.Backtraces]
            bts = Control.Exception.Context.getExceptionAnnotations ctx
        in case bts of
             bt : _ -> case GHC.Internal.Exception.Backtrace.btrHasCallStack bt of
               Data.Maybe.Just cs -> fromCallStack cs
               Data.Maybe.Nothing -> Data.Maybe.Nothing
             [] -> Data.Maybe.Nothing
  in collectExceptionInfo
  """
