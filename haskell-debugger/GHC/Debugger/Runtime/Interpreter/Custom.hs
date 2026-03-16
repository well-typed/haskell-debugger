{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- necessary Binary instances
module GHC.Debugger.Runtime.Interpreter.Custom where

import GHC.Generics (Generic)

import GHCi.RemoteTypes

import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

import GHC.Conc.Sync

--------------------------------------------------------------------------------
-- * Custom Commands
--------------------------------------------------------------------------------

data ThreadInfo ref = ThreadInfo
  { threadInfoRef    :: !(ref ThreadId)
  , threadInfoLabel  :: !(Maybe String)
  , threadInfoStatus :: !ThreadStatus
  }
  deriving (Generic)

data DbgInterpCmd a where
  ListThreads :: DbgInterpCmd [ThreadInfo RemoteRef]

dbgInterpCmdTag :: Word8
dbgInterpCmdTag = 0x25

--------------------------------------------------------------------------------
-- * Running and handlers
--------------------------------------------------------------------------------

-- | Run a custom 'DbgInterpCmd' directly.
runDbgInterpCmd :: DbgInterpCmd a -> IO a
runDbgInterpCmd = \case
  ListThreads -> mapM threadInfo =<< listThreads

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

  get = do
    (tag :: Word8) <- Bin.get
    case tag of
      0 -> pure (Some @Bin.Binary ListThreads)
      _ -> fail ("Unknown debugger thread command tag: " ++ show tag)

instance Bin.Binary (ThreadInfo RemoteRef)

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
