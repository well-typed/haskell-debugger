{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Development.Debug.Adapter.ExceptionInfo
  ( commandExceptionInfo
  ) where

import qualified Data.Text as T

import DAP

import Development.Debug.Adapter
import Development.Debug.Adapter.Interface
import qualified GHC.Debugger.Interface.Messages as D

commandExceptionInfo :: DebugAdaptor ()
commandExceptionInfo = do
  ExceptionInfoArguments{..} <- getArguments
  let remoteThread = D.RemoteThreadId exceptionInfoArgumentsThreadId
  D.GotExceptionInfo info <- sendSync (D.GetExceptionInfo remoteThread)
  sendExceptionInfoResponse (toDAPExceptionInfo info)

-- | Convert the debugger's 'ExceptionInfo' into a DAP 'ExceptionInfoResponse'.
toDAPExceptionInfo :: D.ExceptionInfo -> ExceptionInfoResponse
toDAPExceptionInfo info =
  let typeNameStr = exceptionTypeName info
      typeNameText = T.pack typeNameStr
      messageStr = exceptionMessage info
      messageText = T.pack <$> messageStr
  in ExceptionInfoResponse
      { exceptionInfoResponseExceptionId = typeNameText
      , exceptionInfoResponseDescription = messageText
      , exceptionInfoResponseBreakMode = Always
      , exceptionInfoResponseDetails = Just (exceptionInfoToDetails (Just "_exception") info)
      }

exceptionInfoToDetails :: Maybe T.Text -> D.ExceptionInfo -> ExceptionDetails
exceptionInfoToDetails evalName info@D.ExceptionInfo{..} =
  let typeNameText = T.pack (exceptionTypeName info)
      fullTypeNameText = T.pack (exceptionFullTypeName info)
      stackTraceText = T.pack <$> exceptionInfoContext
      innerDetails = map (exceptionInfoToDetails Nothing) exceptionInfoInner
      innerField = if null innerDetails then Nothing else Just innerDetails
  in defaultExceptionDetails
        { exceptionDetailsMessage = exceptionMessage info
        , exceptionDetailstypeName = Just typeNameText
        , exceptionDetailsFullTypeName = Just fullTypeNameText
        , exceptionDetailsStackTrace = stackTraceText
        , exceptionDetailsInnerException = innerField
        , exceptionDetailsEvaluateName = evalName
        }

exceptionTypeName :: D.ExceptionInfo -> String
exceptionTypeName D.ExceptionInfo{..}
  | null exceptionInfoTypeName = "Exception"
  | otherwise = exceptionInfoTypeName

exceptionFullTypeName :: D.ExceptionInfo -> String
exceptionFullTypeName info@D.ExceptionInfo{..}
  | null exceptionInfoFullTypeName = exceptionTypeName info
  | otherwise = exceptionInfoFullTypeName

exceptionMessage :: D.ExceptionInfo -> Maybe String
exceptionMessage D.ExceptionInfo{..}
  | null exceptionInfoMessage = Nothing
  | otherwise = Just exceptionInfoMessage
