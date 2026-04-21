{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.DAP.Orphans () where

import Data.Aeson
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
