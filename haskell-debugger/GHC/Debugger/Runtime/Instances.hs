{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments #-}
module GHC.Debugger.Runtime.Instances where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import GHC
import GHC.Driver.Env
import GHC.Core.DataCon (DataCon, dataConName)
import GHC.Plugins (falseDataCon, trueDataCon, splitFunTy)
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import qualified GHC.Debugger.Logger as Logger
import GHC.Utils.Outputable (text, (<+>), ppr)
import Control.Monad.Reader
import GHC.Core.TyCo.Compare
import GHC.Driver.Config



import GHC.Debugger.Monad

import GHC.Debugger.Runtime.Instances.Discover

data VarValueResult = VarValueResult { varValueResult :: String, varValueResultExpandable :: Bool }

--------------------------------------------------------------------------------
-- * Term parser abstraction
--------------------------------------------------------------------------------

data TermParseError = TermParseError String
  deriving (Eq, Show)

newtype TermParser a = TermParser { runTermParser :: Term -> Debugger (Either TermParseError a) }

liftDebugger :: Debugger a -> TermParser a
liftDebugger action = TermParser $ \_ -> Right <$> action

instance MonadIO TermParser where
  liftIO action = TermParser $ \_ -> Right <$> liftIO action


instance Functor TermParser where
  fmap f (TermParser p) = TermParser $ \term -> fmap (fmap f) (p term)

instance Applicative TermParser where
  pure x = TermParser $ \_ -> pure (Right x)
  TermParser pf <*> TermParser pa = TermParser $ \term -> do
    ef <- pf term
    case ef of
      Left err -> pure (Left err)
      Right f -> fmap (fmap f) (pa term)

instance Monad TermParser where
  TermParser pa >>= f = TermParser $ \term -> do
    ea <- pa term
    case ea of
      Left err -> pure (Left err)
      Right a -> runTermParser (f a) term

instance Alternative TermParser where
  empty = parseError (TermParseError "TermParser.empty")
  TermParser p1 <|> TermParser p2 = TermParser $ \term -> do
    res <- p1 term
    case res of
      Left _ -> p2 term
      success -> pure success

instance MonadFail TermParser where
  fail s = parseError . TermParseError $ s

parseError :: TermParseError -> TermParser a
parseError err = TermParser $ \_ -> pure (Left err)

termTag :: Term -> String
termTag Term{}         = "Term"
termTag Prim{}         = "Prim"
termTag Suspension{}   = "Suspension"
termTag NewtypeWrap{}  = "NewtypeWrap"
termTag RefWrap{}      = "RefWrap"

anyTerm :: TermParser Term
anyTerm = TermParser $ \term -> pure (Right term)

ensureTerm :: TermParser Term
ensureTerm = do
  t <- anyTerm
  case t of
    Term{} -> pure t
    other -> parseError (TermParseError $ "expected Term, got " <> termTag other)

checkType :: Type -> TermParser ()
checkType ty = do
  t <- anyTerm
  unless (termType t `eqType` ty) (parseError (TermParseError "ty mismatch"))

-- | Evaluate the currently focused term
seqTermP :: TermParser a -> TermParser a
seqTermP parser = do
  t <- anyTerm
  hsc_env <- liftDebugger $ getSession
  t' <- liftIO $ seqTerm hsc_env t
  focus t' parser

-- | Change the focus of the term parser onto the specified term.
focus :: Term -> TermParser a -> TermParser a
focus t parser = TermParser $ \_ -> runTermParser parser t


subtermTerm :: Int -> TermParser Term
subtermTerm idx = TermParser $ \case
  Term{subTerms}
    | idx < length subTerms -> do
        pure (Right (subTerms !! idx))
    | otherwise -> pure (Left (TermParseError $ "missing subterm index " <> show idx))
  other -> pure (Left (TermParseError $ "expected Term with subterms, got " <> termTag other))

subtermWith :: Int -> TermParser a -> TermParser a
subtermWith idx parser = do
  t <- subtermTerm idx
  focus t (seqTermP parser)

tuple2Of :: TermParser a -> TermParser b -> TermParser (a, b)
tuple2Of parserA parserB = (,) <$> subtermWith 0 parserA <*> subtermWith 1 parserB

boolParser :: TermParser Bool
boolParser = do
  Term{dc} <- ensureTerm
  case dc of
    Left "False" -> pure False
    Left "True"  -> pure True
    Right dc'
      | dc' == falseDataCon -> pure False
      | dc' == trueDataCon  -> pure True
    _ -> parseError (TermParseError "expected Bool term")

newtypeWrapParser :: TermParser Term
newtypeWrapParser = do
  t <- anyTerm
  case t of
    NewtypeWrap{wrapped_term} -> pure wrapped_term
    other -> parseError (TermParseError $ "expected NewtypeWrap, got " <> termTag other)

varValueParser :: TermParser (Term, Bool)
varValueParser =
  (,) <$> subtermWith 0 programTermParser <*> subtermWith 1 boolParser

varFieldTupleParser :: TermParser (Term, Term)
varFieldTupleParser = tuple2Of anyTerm anyTerm

varFieldValueParser :: TermParser Term
varFieldValueParser = subtermTerm 0

data ProgramTerm
  = ProgramPure Term
  | ProgramAp ProgramTerm ProgramTerm

evalApplication :: ForeignHValue -> ForeignHValue -> Debugger ForeignHValue
evalApplication fref aref = do
  hsc_env <- getSession
  mk_list_fv <- compileExprRemote "(pure @IO . (:[])) :: a -> IO [a]"

  let eval_opts = initEvalOpts (hsc_dflags hsc_env) EvalStepNone
      interp = hscInterp hsc_env

      handleStatus (EvalComplete _ (EvalSuccess [res])) = res

  liftIO $ handleStatus <$> (evalStmt interp eval_opts $ (EvalThis mk_list_fv) `EvalApp` ((EvalThis fref) `EvalApp` (EvalThis aref)))



-- Parses and evaluates a Term parser
programTermParser :: TermParser Term
programTermParser = do
  term <- ensureTerm
  case term of
    t@Term{} ->
      case constructorName (dc t) of
        "PureProgram" -> subtermTerm 0
        "ProgramAp" -> do
          p1 <- subtermWith 0 programTermParser
          p2 <- subtermWith 1 programTermParser
          let fref1 = val p1
          let fref2 = val p2
          liftDebugger $ logSDoc Logger.Debug (ppr (termType p1))
          liftDebugger $ logSDoc Logger.Debug (ppr (termType p2))
          let (_, arg_ty, res_ty) = splitFunTy (termType p1)
          res <- liftDebugger $ evalApplication fref1 fref2
          hsc_env <- liftDebugger $ getSession
          liftDebugger $ liftIO $ cvObtainTerm hsc_env 2 False res_ty res

        other ->
          parseError $ TermParseError ("expected Program term, got constructor " <> other)
    other ->
      parseError (TermParseError ("expected Program term, got " <> termTag other))
  where
    constructorName :: Either String DataCon -> String
    constructorName = \case
      Left name -> name
      Right dataCon -> occNameString . nameOccName $ dataConName dataCon

logTermParserMsg :: String -> String -> Debugger ()
logTermParserMsg label msg =
  logSDoc Logger.Debug (text "[TermParser]" <+> text label <+> text msg)

runTermParserLogged
  :: String
  -> TermParser a
  -> Term
  -> Debugger (Either TermParseError a)
runTermParserLogged label parser term = do
  logTermParserMsg label "start"
  res <- runTermParser parser term
  case res of
    Left err@(TermParseError errMsg) -> do
      logTermParserMsg label ("failed: " ++ errMsg)
      pure (Left err)
    Right a -> do
      logTermParserMsg label "succeeded"
      pure (Right a)

obtainParsedTerm
  :: String
  -> Int
  -> Bool
  -> Type
  -> ForeignHValue
  -> TermParser a
  -> Debugger (Either TermParseError a)
obtainParsedTerm label depth force ty fhv parser = do
  hsc_env <- getSession
  term <- liftIO $ cvObtainTerm hsc_env depth force ty fhv
  runTermParserLogged label (checkType ty *> parser) term

--------------------------------------------------------------------------------
-- * High level interface for 'DebugView' on 'Term's
--------------------------------------------------------------------------------

-- | Get the custom representation of this 'Term' by applying a 'DebugView'
-- instance 'debugValue' method if there is one.
debugValueTerm :: Term -> Debugger (Maybe VarValueResult)
debugValueTerm term = do
  hsc_env <- getSession
  let interp = hscInterp hsc_env
  let ty = termType term
  mbInst <- getDebugViewInstance ty
  case mbInst of
    Nothing -> return Nothing
    Just DebugViewInstance
      {instDebugValue, varValueIOTy} -> do
        liftIO (instDebugValue (val term)) >>= \case
          Left _e ->
            -- exception! ignore.
            return Nothing
          Right transformed_v -> do

            obtainParsedTerm "VarValue" maxBound True varValueIOTy transformed_v varValueParser >>= \case
              Left _ ->
                return Nothing
              Right (strTerm, valBool) -> do
                valStr <- liftIO $
                  evalString interp (val strTerm {- whose type is IO String, from varValueIO -})

                return $ Just VarValueResult
                  { varValueResult = valStr
                  , varValueResultExpandable = valBool
                  }

-- | Get the custom representation of this 'Term' by applying a 'DebugView'
-- instance 'debugFields' method if there is one.
--
-- Returns the mappings from field labels to terms, where each term records the
-- type and pointer to the foreign heap value returned in the instance for that label.
--
-- Returns @Nothing@ if no instance was found for the type of the given term
debugFieldsTerm :: Term -> Debugger (Maybe [(String, Term)])
debugFieldsTerm term = do
  hsc_env <- getSession
  let interp = hscInterp hsc_env
  let ty = termType term
  mbInst <- getDebugViewInstance ty
  case mbInst of
    Nothing -> return Nothing
    Just DebugViewInstance
      {instDebugFields, varFieldsIOTy} -> do
        liftIO (instDebugFields (val term)) >>= \case
          Left _e ->
            -- exception! ignore.
            return Nothing
          Right transformed_v -> do

            obtainParsedTerm "VarFields" 2 True varFieldsIOTy transformed_v newtypeWrapParser >>= \case
              Left _ -> error "debugFields instance returned something other than VarFields"
              Right fieldsListTerm -> do

                fieldsTerms <- listTermToTermsList fieldsListTerm

                -- Process each term for the instance fields
                Just <$> forM fieldsTerms \fieldTerm0 -> do
                  -- Expand @(IO String, VarFieldValue)@ tuple term for each field
                  fieldTerm <- liftIO $ seqTerm hsc_env fieldTerm0
                  runTermParserLogged "VarFieldTuple" varFieldTupleParser fieldTerm >>= \case
                    Left _ -> error "impossible; expected 2-tuple term"
                    Right (ioStrTerm, varFieldValTerm) -> do

                      fieldStr <- liftIO $ evalString interp (val ioStrTerm)

                      -- Expand VarFieldValue term
                      varFieldValTerm' <- liftIO $ seqTerm hsc_env varFieldValTerm
                      runTermParserLogged "VarFieldValueWrapper" varFieldValueParser varFieldValTerm' >>= \case
                        Left _ -> error "impossible; expected VarFieldValue"
                        Right unexpandedValueTerm -> do
                          let val_ty = termType unexpandedValueTerm
                          actualValueTerm <-
                            obtainParsedTerm "VarFieldValue" defaultDepth False{-don't force-} val_ty (val unexpandedValueTerm) anyTerm >>= \case
                              Left _ -> error "Failed to obtain VarFieldValue term"
                              Right termValue -> pure termValue
                          return (fieldStr, actualValueTerm)

-- | Convert a Term representing a list @[a]@ to a list of the terms of type
-- @a@, where @a@ is the given @'Type'@ arg.
--
-- PRE-CON: Term represents a @[a]@
listTermToTermsList :: Term -> Debugger [Term]
listTermToTermsList Term{subTerms=[head_term, tail_term]}
  = do
    hsc_env <- getSession
    -- Expand next term:
    tail_term' <- liftIO $
      seqTerm hsc_env tail_term
    (head_term:) <$> listTermToTermsList tail_term'
listTermToTermsList _ = pure []
