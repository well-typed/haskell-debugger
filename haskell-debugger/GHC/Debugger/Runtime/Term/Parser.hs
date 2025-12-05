{-# LANGUAGE LambdaCase, BlockArguments #-}
-- | This module contains the 'TermParser' abstraction, which provides utilities for
-- interpreting and parsing 'Term's
module GHC.Debugger.Runtime.Term.Parser where

import Control.Applicative
import Control.Monad
import Control.Exception

import GHC
import GHC.Driver.Env
import GHC.Core.DataCon (dataConName)
import GHC.Plugins (falseDataCon, trueDataCon, splitFunTy, boolTy)
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import qualified GHC.Debugger.Logger as Logger
import GHC.Utils.Outputable (text, (<+>), ppr)
import Control.Monad.Reader
import GHC.Core.TyCo.Compare
import GHC.Stack

import GHC.Debugger.Monad
import GHC.Debugger.Runtime.Eval
import GHC.Debugger.Utils (expectRight)

-- | The main entry point for running the 'TermParser'.
obtainParsedTerm
  :: String
  -> Int
  -> Bool
  -> Type
  -> ForeignHValue
  -> TermParser a
  -> Debugger (Either [TermParseError] a)
obtainParsedTerm label depth force ty fhv term_parser = do
  hsc_env <- getSession
  term <- liftIO $ cvObtainTerm hsc_env depth force ty fhv
  runTermParserLogged label (checkType ty *> term_parser) term


--------------------------------------------------------------------------------
-- * Term parser abstraction
--------------------------------------------------------------------------------

data TermParseError = TermParseError { getTermErrorMessage :: String }
  deriving (Eq, Show)

newtype TermParser a = TermParser { runTermParser :: Term -> Debugger (Either [TermParseError] a) }

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
      Left e1 -> attachErrors e1 $ p2 term
      success -> pure success

instance MonadFail TermParser where
  fail s = parseError . TermParseError $ s

attachErrors :: [TermParseError] -> Debugger (Either [TermParseError] a)
                                 -> Debugger (Either [TermParseError] a)
attachErrors errs term_parser = do
  pres <- term_parser
  case pres of
    Left new_errs -> pure (Left $ errs ++ new_errs)
    Right res -> pure (Right res)

parseError :: TermParseError -> TermParser a
parseError err = TermParser $ \_ -> pure (Left [err])

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

traceTerm :: TermParser ()
traceTerm = do
  t <- anyTerm
  liftDebugger $ logSDoc Logger.Debug (ppr t)

-- | Evaluate the currently focused term
seqTermP :: HasCallStack => TermParser a -> TermParser a
seqTermP term_parser = do
  t <- anyTerm
  hsc_env <- liftDebugger $ getSession
  focus (liftIO $ seqTerm hsc_env t)
        term_parser

-- | If a term is a suspension, make sure that it's a thunk and not just that we
-- reached the depth limit.
refreshTerm :: TermParser Term
refreshTerm = do
  t <- anyTerm
  case t of
    Suspension {} -> do
      t' <- foreignValueToTerm (ty t) (val t)
      return t'
    _ -> return t


-- | Change the focus of the term parser onto the specified term.
focus :: TermParser Term -> TermParser a -> TermParser a
focus parse_term term_parser =
  parse_term >>= \t ->
    TermParser $ \_ -> runTermParser term_parser t

-- | Focus on a new subtree, after forcing it to WHNF.
focusSeq :: HasCallStack => TermParser Term -> TermParser a -> TermParser a
focusSeq parse_term term_parser = focus parse_term (seqTermP term_parser)

-- | Choose the nth subterm
subtermTerm :: Int -> TermParser Term
subtermTerm idx = do
  t <- anyTerm
  case t of
    Term{subTerms}
      | idx < length subTerms -> do
          liftDebugger $ logSDoc Logger.Debug (ppr subTerms)
          pure (subTerms !! idx)
      | otherwise -> parseError (TermParseError $ "missing subterm index " <> show idx)
    other -> parseError (TermParseError $ "expected Term with subterms, got " <> termTag other)

-- | Choose the nth subterm, force it to WHNF and run the supplied parser on it.
subtermWith :: Int -> TermParser a -> TermParser a
subtermWith idx term_parser = do
  focusSeq (subtermTerm idx) term_parser

matchOccNameTerm :: String -> a -> TermParser a
matchOccNameTerm occName result = do
  Term{dc} <- ensureTerm
  case dc of
    Left name | name == occName -> pure result
    _ -> empty

matchDataConTerm :: DataCon -> a -> TermParser a
matchDataConTerm dataCon result = do
  Term{dc} <- ensureTerm
  case dc of
    Right dc' | dc' == dataCon -> pure result
    _ -> empty

matchConstructorTerm :: String -> TermParser ()
matchConstructorTerm ctorName = do
  term <- anyTerm
  case term of
    t@Term{} | constructorName (dc t) == ctorName -> return ()
             | otherwise ->
                parseError (TermParseError ("expected: "
                                            ++ ctorName
                                            ++ " got: "
                                            ++ constructorName (dc t)))
    other ->
      parseError (TermParseError ("expected Program term, got " <> termTag other))

constructorName :: Either String DataCon -> String
constructorName = \case
  Left name -> name
  Right dataCon -> occNameString . nameOccName $ dataConName dataCon

newtypeWrapParser :: TermParser Term
newtypeWrapParser = do
  t <- anyTerm
  case t of
    NewtypeWrap{wrapped_term} -> pure wrapped_term
    other -> parseError (TermParseError $ "expected NewtypeWrap, got " <> termTag other)

-- | Parse a primitive value as a single word (Prim term)
primParser :: TermParser Word
primParser = do
  t <- anyTerm
  case t of
    Prim{valRaw=[w64_tid]} -> pure w64_tid
    other -> parseError (TermParseError $ "expected a Prim term, got " <> termTag other)

-- | Is the current focus a suspension?
isSuspension :: TermParser Bool
isSuspension = focus refreshTerm $ do
  t <- anyTerm
  traceTerm
  case t of
    Suspension{} -> pure True
    other -> do
      liftDebugger $ logSDoc Logger.Debug (text $ termTag other)
      return False

-- | Obtain a Term from a ForeignHValue
foreignValueToTerm :: Type -> ForeignHValue -> TermParser Term
foreignValueToTerm ty fhv =
  liftDebugger $ do
    hsc_env <- getSession
    liftIO $ cvObtainTerm hsc_env 2 False ty fhv

--------------------------------------------------------------------------------
-- * Logging parsers
--------------------------------------------------------------------------------

logTermParserMsg :: String -> String -> Debugger ()
logTermParserMsg label msg =
  logSDoc Logger.Debug (text "[TermParser]" <+> text label <+> text msg)

runTermParserLogged
  :: String
  -> TermParser a
  -> Term
  -> Debugger (Either [TermParseError] a)
runTermParserLogged label term_parser term = do
  logTermParserMsg label "start"
  res <- runTermParser term_parser term
  case res of
    Left errs -> do
      logTermParserMsg label ("failed: " ++ unlines (map getTermErrorMessage errs))
      pure (Left errs)
    Right a -> do
      logTermParserMsg label "succeeded"
      pure (Right a)

--------------------------------------------------------------------------------
-- * Base parsers
--------------------------------------------------------------------------------

tuple2Of :: TermParser a -> TermParser b -> TermParser (a, b)
tuple2Of parserA parserB = (,) <$> subtermWith 0 parserA <*> subtermWith 1 parserB

boolParser :: TermParser Bool
boolParser =
  matchOccNameTerm "False" False
    <|> matchOccNameTerm "True" True
    <|> matchDataConTerm falseDataCon False
    <|> matchDataConTerm trueDataCon True
    <|> parseError (TermParseError "expected Bool term")

-- | Parse a list, given a parser for each element.
-- The whole list will be forced.
parseList :: TermParser a -> TermParser [a]
parseList item_parser =
        (matchConstructorTerm "[]" *> pure [])
    <|> (matchConstructorTerm ":" *> ((:) <$> subtermWith 0 item_parser <*> subtermWith 1 (parseList item_parser)))

-- | Parse an 'Int'
intParser :: TermParser Int
intParser = fromIntegral <$> subtermWith 0 primParser

-- | God...
stringParser :: TermParser String
stringParser = do
  Term{val=string_fv} <- anyTerm
  liftDebugger $ do
    pure_fv      <- compileExprRemote "(pure @IO) :: String -> IO String"
    string_io_fv <- expectRight =<< evalApplication pure_fv string_fv
    hsc_env      <- getSession
    liftIO $ evalString (hscInterp hsc_env) string_io_fv

--------------------------------------------------------------------------------
-- * VarValue
--------------------------------------------------------------------------------

-- | Parse a term which is a 'ValValue'
varValueParser :: TermParser (Term, Bool)
varValueParser =
  (,) <$> subtermWith 0 programTermParser <*> subtermWith 1 boolParser

--------------------------------------------------------------------------------
-- * VarFields
--------------------------------------------------------------------------------

-- | Parse a term which is a 'Program VarFields'
varFieldsParser :: TermParser [(String, Term)]
varFieldsParser =
  focusSeq newtypeWrapParser $
    -- Program [(IO String, VarFieldValue)]
    focusSeq programTermParser $
        -- [(IO String, VarFieldValue)]
        parseList parseFieldItem

  where
    -- Parses an item of type (IO String, VarFieldValue)
    parseFieldItem :: TermParser (String, Term)
    parseFieldItem = (,) <$> subtermWith 0 parseFieldLabel <*> subtermWith 1 varFieldValueParser

    parseFieldLabel :: TermParser String
    parseFieldLabel = do
      ioStrTerm <- anyTerm
      liftDebugger $ do
        interp <- hscInterp <$> getSession
        liftIO $ evalString interp (val ioStrTerm)

varFieldTupleParser :: TermParser (Term, Term)
varFieldTupleParser = tuple2Of anyTerm anyTerm

varFieldValueParser :: TermParser Term
varFieldValueParser = subtermTerm 0

--------------------------------------------------------------------------------
-- * Program Parser
--------------------------------------------------------------------------------

-- | Parses and evaluates a "Program" term.
programTermParser :: TermParser Term
programTermParser =
        programPureParser
    <|> programApParser
    <|> programBranchParser
    <|> programAskThunkParser
  where
    programPureParser = do
      matchConstructorTerm "PureProgram"
      subtermTerm 0

    programApParser = do
      matchConstructorTerm "ProgramAp"
      p1 <- subtermWith 0 programTermParser
      p2 <- subtermWith 1 programTermParser
      let fref1 = val p1
      let fref2 = val p2
      let (_, _arg_ty, res_ty) = splitFunTy (termType p1)
      res <- liftDebugger $ expectRight =<< evalApplication fref1 fref2
      foreignValueToTerm res_ty res

    programBranchParser = do
      matchConstructorTerm "ProgramBranch"
      cond <- subtermWith 0 (focusSeq programTermParser boolParser)
      if cond then do
            subtermWith 1 programTermParser
           else do
            subtermWith 2 programTermParser

    programAskThunkParser = do
      matchConstructorTerm "ProgramAskThunk"
      -- Get what we need to check THUNKiness for
      is_thunk <- focus (subtermTerm 1) isSuspension
      bool_fv <- liftDebugger $ reifyBool is_thunk
      foreignValueToTerm boolTy bool_fv

reifyBool :: Bool -> Debugger ForeignHValue
reifyBool b = compileExprRemote (show b ++ ":: Bool")

