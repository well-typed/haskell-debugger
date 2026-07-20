{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Development.Debug.Adapter where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (liftBaseDiscard)
import qualified Data.IntSet as IS
import qualified Data.Map as Map
import qualified Data.IntMap as IM
import qualified Data.Text as T
import System.Directory (getCurrentDirectory)
import System.FilePath

import DAP
import qualified GHC
import qualified GHC.Debugger.Interface.Messages as D (Command, Response, RemoteThreadId, VariableReference)
import GHC.Debugger.Interface.Messages (AbsFilePath, unAbs, (/>), mkAbsolute)

type DebugAdaptor = Adaptor DebugAdaptorState Request
type DebugAdaptorCont = Adaptor DebugAdaptorState ()
type DebugAdaptorX r = Adaptor DebugAdaptorState r ()

-- | Debugger state:
--
-- * Keep a mapping from DAP breakpoint ids to internal breakpoint ids
-- * Keep the MVar through which synchronous communication with the debugger is done.
--    - The debugger main worker writes to this MVar responses (and, for now, events too)
--    - The handler worker reads from this MVar and writes them to the client with the 'Adapter'.
data DebugAdaptorState = DAS
      { syncRequests  :: MVar D.Command
      , syncResponses :: MVar D.Response
      , nextFreshId   :: !Int
      , breakpointMap :: Map.Map GHC.InternalBreakpointId BreakpointSet
      , stackFrameMap :: IM.IntMap StackFrameIx
      , variablesMap  :: IM.IntMap VariablesIx
      , entryFile     :: AbsFilePath
      , entryPoint    :: String
      , entryArgs     :: [String]
      , projectRoot   :: AbsFilePath
      , waitForDebuggee :: IO ()
        -- ^ Blocks until the debuggee is ready for @startExecution@.
      }

type BreakpointId = Int
type BreakpointSet = IS.IntSet

data StackFrameIx = StackFrameIx D.RemoteThreadId Int{-stack frame ix-}
  deriving (Eq, Ord)
data VariablesIx = VariablesIx StackFrameIx D.VariableReference

instance MonadFail DebugAdaptor where
  fail a = sendError (ErrorMessage (T.pack a)) Nothing

safeDestroyDebugSession :: Adaptor app request ()
safeDestroyDebugSession = void $ do
  -- Without forkIO we might kill ourselves first and not kill anything else.
  liftBaseDiscard forkIO $
    destroyDebugSession `catchError` \ e -> liftIO $ putStrLn ("safeDestroyDebugSession: ignoring missing session: " ++ show e)

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

-- | Transform the given file into a DAP 'Source'. The file may be modified:
--
--    * if the given filepath is absolute, it's returned unchanged
--    * if it is relative it's made absolute by prepending the current directory.
fileToSource :: FilePath -> DebugAdaptor Source
fileToSource file = do
  fullPath <- if isAbsolute file
     then return file
     else do
      root <- liftIO $ mkAbsolute <$> getCurrentDirectory
      return (unAbs $ root /> file)
  return defaultSource{sourcePath = Just (T.pack fullPath)}

-- | Generate fresh Int identifier.
getFreshId :: DebugAdaptor Int
getFreshId = do
  nid <- nextFreshId <$> getDebugSession
  updateDebugSession $ \s -> s { nextFreshId = nextFreshId s + 1 }
  pure nid
