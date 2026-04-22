{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Test.DAP.Orphans () where

import Data.Aeson
import qualified Data.Text as T
import DAP.Utils (genericParseJSONWithModifier, genericToJSONWithModifier)
import DAP

--------------------------------------------------------------------------------
-- * FromJSON instances for response body types (client parses server responses)
--------------------------------------------------------------------------------

instance FromJSON Breakpoint where
  parseJSON = genericParseJSONWithModifier

instance FromJSON PresentationHint where
  parseJSON = genericParseJSONWithModifier

instance FromJSON StackFrame where
  parseJSON = genericParseJSONWithModifier

instance FromJSON Scope where
  parseJSON = genericParseJSONWithModifier

instance FromJSON ScopePresentationHint where
  parseJSON = withText "ScopePresentationHint" $ \t -> pure $ case t of
    "arguments" -> ScopePresentationHintArguments
    "locals"    -> ScopePresentationHintLocals
    "registers" -> ScopePresentationHintRegisters
    other       -> ScopePresentationHint other

instance FromJSON StoppedEventReason where
  parseJSON = withText "StoppedEventReason" $ \t -> case t of
    "step"                   -> pure StoppedEventReasonStep
    "breakpoint"             -> pure StoppedEventReasonBreakpoint
    "exception"              -> pure StoppedEventReasonException
    "pause"                  -> pure StoppedEventReasonPause
    "entry"                  -> pure StoppedEventReasonEntry
    "goto"                   -> pure StoppedEventReasonGoto
    "function breakpoint"    -> pure StoppedEventReasonFunctionBreakpoint
    "data breakpoint"        -> pure StoppedEventReasonDataBreakpoint
    "instruction breakpoint" -> pure StoppedEventReasonInstructionBreakpoint
    other                    -> fail $ "Unknown StoppedEventReason: " ++ show other

instance FromJSON StoppedEvent where
  parseJSON = genericParseJSONWithModifier

instance FromJSON OutputEventCategory where
  parseJSON = withText "OutputEventCategory" $ \t -> pure $ case t of
    "console"   -> OutputEventCategoryConsole
    "important" -> OutputEventCategoryImportant
    "stdout"    -> OutputEventCategoryStdout
    "stderr"    -> OutputEventCategoryStderr
    "telemetry" -> OutputEventCategoryTelemetry
    other       -> OutputEventCategory other

instance FromJSON ThreadsResponse where
  parseJSON = withObject "fromJSON:ThreadsResponse" $ \o -> do
    ts <- o .: "threads"
    pure $ ThreadsResponse ts

instance FromJSON StackTraceResponse where
  parseJSON = withObject "fromJSON:StackTraceResponse" $ \o -> do
    fs <- o .: "stackFrames"
    tot <- o .:? "totalFrames"
    pure $ StackTraceResponse fs tot

instance FromJSON ScopesResponse where
  parseJSON = withObject "fromJSON:ScopesResponse" $ \o -> do
    scs <- o .: "scopes"
    pure $ ScopesResponse scs

instance FromJSON EventGroup where
  parseJSON = genericParseJSONWithModifier

instance FromJSON OutputEvent where
  parseJSON = genericParseJSONWithModifier

instance FromJSON Variable where
  parseJSON = genericParseJSONWithModifier

instance FromJSON VariablePresentationHint where
  parseJSON = genericParseJSONWithModifier

instance FromJSON PresentationHintKind where
  parseJSON = withText "PresentationHintKind" $ \t -> pure $ case t of
    "property"          -> PresentationHintKindProperty
    "method"            -> PresentationHintKindMethod
    "class"             -> PresentationHintKindClass
    "data"              -> PresentationHintKindData
    "event"             -> PresentationHintKindEvent
    "baseClass"         -> PresentationHintKindBaseClass
    "innerClass"        -> PresentationHintKindInnerClass
    "interface"         -> PresentationHintKindInterface
    "mostDerivedClass"  -> PresentationHintKindMostDerivedClass
    "virtual"           -> PresentationHintKindVirtual
    "dataBreakpoint"    -> PresentationHintKindDataBreakpoint
    other               -> PresentationHintKind other

instance FromJSON PresentationHintVisibility where
  parseJSON = withText "PresentationHintVisibility" $ \t -> pure $ case t of
    "public"    -> PresentationHintVisibilityPublic
    "private"   -> PresentationHintVisibilityPrivate
    "protected" -> PresentationHintVisibilityProtected
    "internal"  -> PresentationHintVisibilityInternal
    "final"     -> PresentationHintVisibilityFinal
    other       -> PresentationHintVisibility (T.unpack other)

instance FromJSON PresentationHintAttributes where
  parseJSON = withText "PresentationHintAttributes" $ \t -> pure $ case t of
    "static"            -> PresentationHintAttributesStatic
    "constant"          -> PresentationHintAttributesConstant
    "readOnly"          -> PresentationHintAttributesReadOnly
    "rawText"           -> PresentationHintAttributesRawText
    "hasObjectId"       -> PresentationHintAttributesHasObjectId
    "canHaveObjectId"   -> PresentationHintAttributesCanHaveObjectId
    "hasSideEffects"    -> PresentationHintAttributesHasSideEffects
    "hasDataBreakpoint" -> PresentationHintAttributesHasDataBreakpoint
    other               -> PresentationHintAttributes (T.unpack other)

instance FromJSON VariablesResponse where
  parseJSON = withObject "fromJSON:VariablesResponse" $ \o -> do
    vs <- o .: "variables"
    pure $ VariablesResponse vs

instance FromJSON EvaluateResponse where
  parseJSON = genericParseJSONWithModifier

instance FromJSON ExceptionInfoResponse where
  -- NB: we parse 'breakMode' manually because the stock 'genericParseJSON'
  -- of the 'ExceptionBreakMode' sum type (as defined in DAP.Types) does not
  -- correctly map the JSON tag "never"/"always"/"unhandled"/"userUnhandled"
  -- to the corresponding constructors when the constructor name lacks a
  -- common prefix.
  parseJSON = withObject "ExceptionInfoResponse" $ \o -> do
    exceptionId  <- o .: "exceptionId"
    description  <- o .:? "description"
    breakModeTxt <- o .: "breakMode"
    bm <- case (breakModeTxt :: T.Text) of
      "never"         -> pure Never
      "always"        -> pure Always
      "unhandled"     -> pure Unhandled
      "userUnhandled" -> pure UserUnhandled
      other           -> fail $ "Unknown ExceptionBreakMode: " ++ show other
    details      <- o .:? "details"
    pure ExceptionInfoResponse
      { exceptionInfoResponseExceptionId = exceptionId
      , exceptionInfoResponseDescription = description
      , exceptionInfoResponseBreakMode   = bm
      , exceptionInfoResponseDetails     = details
      }

instance FromJSON ExceptionDetails where
  parseJSON = genericParseJSONWithModifier

instance FromJSON ExitedEvent where
  parseJSON = genericParseJSONWithModifier

--------------------------------------------------------------------------------
-- * ToJSON instances for request argument types (client sends requests)
--------------------------------------------------------------------------------

instance ToJSON PathFormat where
  toJSON Path = "path"
  toJSON URI  = "uri"
  toJSON (PathFormat t) = toJSON t

instance ToJSON InitializeRequestArguments where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance ToJSON SetBreakpointsArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON SourceBreakpoint where
  toJSON = genericToJSONWithModifier

instance ToJSON SteppingGranularity where
  toJSON = genericToJSONWithModifier

instance ToJSON NextArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON StackFrameFormat where
  toJSON = genericToJSONWithModifier

instance ToJSON StackTraceArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON ScopesArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON ValueFormat where
  toJSON = genericToJSONWithModifier

instance ToJSON DisconnectArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON VariablesArguments where
  toJSON VariablesArguments{..} = object $
    [ "variablesReference" .= variablesArgumentsVariablesReference
    ] ++
    [ "start" .= v | Just v <- [variablesArgumentsStart] ] ++
    [ "count" .= v | Just v <- [variablesArgumentsCount] ] ++
    [ "format" .= v | Just v <- [variablesArgumentsFormat] ]

instance ToJSON StepOutArguments where
  toJSON StepOutArguments{..} = object $
    [ "threadId" .= stepOutArgumentsThreadId
    , "singleThread" .= stepOutArgumentsSingleThread
    ] ++
    [ "granularity" .= g | Just g <- [stepOutArgumentsGranularity] ]

instance ToJSON ContinueArguments where
  toJSON ContinueArguments{..} = object
    [ "threadId" .= continueArgumentsThreadId
    , "singleThread" .= continueArgumentsSingleThread
    ]

instance ToJSON EvaluateArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON SetExceptionBreakpointsArguments where
  toJSON = genericToJSONWithModifier

instance ToJSON ExceptionFilterOptions where
  toJSON = genericToJSONWithModifier

instance ToJSON ExceptionOptions where
  toJSON = genericToJSONWithModifier

instance ToJSON ExceptionPathSegment where
  toJSON = genericToJSONWithModifier

instance ToJSON ExceptionInfoArguments where
  toJSON = genericToJSONWithModifier
