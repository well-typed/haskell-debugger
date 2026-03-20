{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}

-- | Helpers used when the debugger is stopped due to an exception.
-- These helpers execute code on the remote process which teach us information
-- about the exception we are stopped at.
module GHC.Debugger.Stopped.Exception
  ( getExceptionInfo
  , defaultExceptionInfo
  , currentlyStoppedOnException
  ) where

import Data.Maybe (fromMaybe, isNothing)

import GHC
import GHC.Types.SrcLoc
import GHC.Data.FastString (unpackFS)
import GHC.Utils.Outputable as Ppr

import GHC.Debugger.Monad
import GHC.Debugger.Interface.Messages
  ( SourceSpan(..)
  , ExceptionInfo(..)
  , RemoteThreadId(..)
  )
import qualified Colog.Core as Logger
import GHC.Debugger.Runtime.Thread
import qualified GHC.Debugger.Runtime.Eval.RemoteExpr as Remote
import GHC.Debugger.Runtime.Term.Parser
import GHCi.RemoteTypes (castForeignRef)
import GHC.Builtin.Types (anyTy)

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
        fromMaybe defaultExceptionInfo <$> exceptionInfoFromContext excRef
      _ -> return defaultExceptionInfo

-- | Evaluate helper code inside the debuggee that turns the exception context
-- into our 'ExceptionInfo' structure.
exceptionInfoFromContext :: ForeignHValue -> Debugger (Maybe ExceptionInfo)
exceptionInfoFromContext excRef = do
  -- 1. Add a "data" declaration for the datatype the expression will return
  _ <- runDecls exceptionInfoData
  -- 2. Gather information about the exception.
  evalRes <- Remote.eval
    (Remote.raw exceptionInfoExpr `Remote.app` Remote.untypedRef excRef)
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
  ExceptionInfo
    <$> subtermWith 0 stringParser
    <*> subtermWith 1 stringParser
    <*> subtermWith 2 stringParser
    <*> subtermWith 3 (maybeParser stringParser)
    <*> subtermWith 4 (maybeParser exceptionLocationTupleParser)
    <*> subtermWith 5 (parseList exceptionInfoParser)
  where
    -- Parsed from @(String, Int, Int)@.
    exceptionLocationTupleParser :: TermParser SourceSpan
    exceptionLocationTupleParser = do
      locFile <- subtermWith 0 stringParser
      srcLine <- subtermWith 1 intParser
      srcCol <- subtermWith 2 intParser
      pure SourceSpan
        { file = locFile
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

-- | Determine whether the debugger is currently stopped because of an
-- exception (as opposed to a breakpoint).
currentlyStoppedOnException :: Debugger Bool
currentlyStoppedOnException = do
  resumes <- GHC.getResumeContext
  return $ case resumes of
    [] -> False
    r:_ -> isNothing (GHC.resumeBreakpointId r)
