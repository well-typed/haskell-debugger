{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments #-}
module GHC.Debugger.Runtime.Instances where

import Control.Applicative
import Control.Monad

import GHC
import GHC.Driver.Env
import GHC.Plugins (falseDataCon, trueDataCon)
import GHC.Runtime.Eval
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Interpreter as Interp
import qualified GHC.Debugger.Logger as Logger
import GHC.Utils.Outputable (text, (<+>))
import Control.Monad.Reader


import GHC.Debugger.Monad

import GHC.Debugger.Runtime.Instances.Discover

data VarValueResult = VarValueResult { varValueResult :: String, varValueResultExpandable :: Bool }

--------------------------------------------------------------------------------
-- * Term parser abstraction
--------------------------------------------------------------------------------

data TermParseError = TermParseError String
  deriving (Eq, Show)

newtype TermParser a = TermParser { runTermParser :: HscEnv -> Term -> IO (Either TermParseError a) }

instance Functor TermParser where
  fmap f (TermParser p) = TermParser $ \env term -> fmap (fmap f) (p env term)

instance Applicative TermParser where
  pure x = TermParser $ \_ _ -> pure (Right x)
  TermParser pf <*> TermParser pa = TermParser $ \env term -> do
    ef <- pf env term
    case ef of
      Left err -> pure (Left err)
      Right f -> fmap (fmap f) (pa env term)

instance Monad TermParser where
  TermParser pa >>= f = TermParser $ \env term -> do
    ea <- pa env term
    case ea of
      Left err -> pure (Left err)
      Right a -> runTermParser (f a) env term

instance Alternative TermParser where
  empty = TermParser $ \_ _ -> pure (Left (TermParseError "TermParser.empty"))
  TermParser p1 <|> TermParser p2 = TermParser $ \env term -> do
    res <- p1 env term
    case res of
      Left _ -> p2 env term
      success -> pure success

termTag :: Term -> String
termTag Term{}         = "Term"
termTag Prim{}         = "Prim"
termTag Suspension{}   = "Suspension"
termTag NewtypeWrap{}  = "NewtypeWrap"
termTag RefWrap{}      = "RefWrap"

anyTerm :: TermParser Term
anyTerm = TermParser $ \_ term -> pure (Right term)

ensureTerm :: Term -> Either TermParseError Term
ensureTerm t@Term{} = Right t
ensureTerm other    = Left (TermParseError $ "expected Term, got " <> termTag other)

subtermTerm :: Int -> TermParser Term
subtermTerm idx = TermParser $ \env -> \case
  Term{subTerms}
    | idx < length subTerms -> do
        term <- seqTerm env (subTerms !! idx)
        pure (Right term)
    | otherwise -> pure (Left (TermParseError $ "missing subterm index " <> show idx))
  other -> pure (Left (TermParseError $ "expected Term with subterms, got " <> termTag other))

subtermWith :: Int -> TermParser a -> TermParser a
subtermWith idx parser = TermParser $ \env term -> do
  childRes <- runTermParser (subtermTerm idx) env term
  case childRes of
    Left err -> pure (Left err)
    Right child -> runTermParser parser env child

tuple2Of :: TermParser a -> TermParser b -> TermParser (a, b)
tuple2Of parserA parserB = TermParser $ \env -> \case
  Term{subTerms=[aTerm,bTerm]} -> do
    aTerm' <- seqTerm env aTerm
    bTerm' <- seqTerm env bTerm
    ea <- runTermParser parserA env aTerm'
    case ea of
      Left err -> pure (Left err)
      Right a -> do
        eb <- runTermParser parserB env bTerm'
        case eb of
          Left err -> pure (Left err)
          Right b -> pure (Right (a, b))
  other -> pure (Left (TermParseError $ "expected 2-tuple Term, got " <> termTag other))

boolParser :: TermParser Bool
boolParser = TermParser $ \_ term ->
  case ensureTerm term of
    Left err -> pure (Left err)
    Right Term{dc} ->
      case dc of
        Left "False" -> pure (Right False)
        Left "True"  -> pure (Right True)
        Right dc'
          | dc' == falseDataCon -> pure (Right False)
          | dc' == trueDataCon  -> pure (Right True)
        _ -> pure (Left (TermParseError "expected Bool term"))

newtypeWrapParser :: TermParser Term
newtypeWrapParser = TermParser $ \_ -> \case
  NewtypeWrap{wrapped_term} -> pure (Right wrapped_term)
  other -> pure (Left (TermParseError $ "expected NewtypeWrap, got " <> termTag other))

varValueParser :: TermParser (Term, Bool)
varValueParser = (,)
  <$> subtermTerm 0
  <*> subtermWith 1 boolParser

varFieldTupleParser :: TermParser (Term, Term)
varFieldTupleParser = tuple2Of anyTerm anyTerm

varFieldValueParser :: TermParser Term
varFieldValueParser = subtermTerm 0

logTermParserMsg :: String -> String -> Debugger ()
logTermParserMsg label msg =
  logSDoc Logger.Debug (text "[TermParser]" <+> text label <+> text msg)

runTermParserLogged
  :: String
  -> TermParser a
  -> HscEnv
  -> Term
  -> Debugger (Either TermParseError a)
runTermParserLogged label parser hsc_env term = do
  logTermParserMsg label "start"
  res <- liftIO (runTermParser parser hsc_env term)
  case res of
    Left err@(TermParseError errMsg) -> do
      logTermParserMsg label ("failed: " ++ errMsg)
      pure (Left err)
    Right a -> do
      logTermParserMsg label "succeeded"
      pure (Right a)

obtainParsedTerm
  :: String
  -> HscEnv
  -> Int
  -> Bool
  -> Type
  -> ForeignHValue
  -> TermParser a
  -> Debugger (Either TermParseError a)
obtainParsedTerm label hsc_env depth force ty fhv parser = do
  term <- liftIO $ cvObtainTerm hsc_env depth force ty fhv
  runTermParserLogged label parser hsc_env term

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

            obtainParsedTerm "VarValue" hsc_env maxBound True varValueIOTy transformed_v varValueParser >>= \case
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

            obtainParsedTerm "VarFields" hsc_env 2 True varFieldsIOTy transformed_v newtypeWrapParser >>= \case
              Left _ -> error "debugFields instance returned something other than VarFields"
              Right fieldsListTerm -> do

                fieldsTerms <- listTermToTermsList fieldsListTerm

                -- Process each term for the instance fields
                Just <$> forM fieldsTerms \fieldTerm0 -> do
                  -- Expand @(IO String, VarFieldValue)@ tuple term for each field
                  fieldTerm <- liftIO $ seqTerm hsc_env fieldTerm0
                  runTermParserLogged "VarFieldTuple" varFieldTupleParser hsc_env fieldTerm >>= \case
                    Left _ -> error "impossible; expected 2-tuple term"
                    Right (ioStrTerm, varFieldValTerm) -> do

                      fieldStr <- liftIO $ evalString interp (val ioStrTerm)

                      -- Expand VarFieldValue term
                      varFieldValTerm' <- liftIO $ seqTerm hsc_env varFieldValTerm
                      runTermParserLogged "VarFieldValueWrapper" varFieldValueParser hsc_env varFieldValTerm' >>= \case
                        Left _ -> error "impossible; expected VarFieldValue"
                        Right unexpandedValueTerm -> do
                          let val_ty = termType unexpandedValueTerm
                          actualValueTerm <-
                            obtainParsedTerm "VarFieldValue" hsc_env defaultDepth False{-don't force-} val_ty (val unexpandedValueTerm) anyTerm >>= \case
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
