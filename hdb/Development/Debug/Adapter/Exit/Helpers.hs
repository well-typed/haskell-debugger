module Development.Debug.Adapter.Exit.Helpers where

import DAP
import Data.Function
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Class (MonadError(..))
import Control.Exception
import Control.Exception.Context
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Development.Debug.Adapter
import qualified Development.Debug.Adapter.Output as Output

--- Exit Cleanly ---------------------------------------------------------------

-- | Outputs a message notification ('Output.important'), sends a terminated
-- event, destroys the debug session, and dies.
--
-- ::WARNING::
--
-- This function should not be called if the debugsession with the debugger
-- threads haven't yet been registered because it WILL block on the call to
-- @'destroyDebugSession'@.
exitCleanupWithMsg
  :: Handle
  -- ^ Handle to finalize reading as OutputEvents before exiting (but after
  -- killing the output thread with @destroyDebugSession@)
  -> String
  -- ^ Error message, logged with notification
  -> DebugAdaptor ()
exitCleanupWithMsg final_handle msg = do
  destroyDebugSession -- kill all session threads (including the output thread)
  has_data <- hReady final_handle & liftIO
  when has_data $ do
      -- get all pending output from GHC
      c <- T.hGetContents final_handle & liftIO
      Output.neutral c
  exitWithMsg msg

-- | Logs the error to the debug console and sends a terminate event
exitWithMsg :: String -> DebugAdaptor ()
exitWithMsg msg = do
  Output.important (T.pack msg)
  terminateSessionCleanly (Just msg)

terminateSessionCleanly :: Maybe String -> DebugAdaptor ()
terminateSessionCleanly mm = do
  -- throws error if no session found.
  destroyDebugSession `catchError` \ e -> liftIO $ hPutStrLn stdout ("terminateSessionCleanly: ignoring missing session: " ++ show e)
  sendTerminatedEvent (TerminatedEvent False)

  liftIO $ do
    case mm of
      Nothing -> return ()
      Just em -> do
        hPutStrLn stderr em
        return ()

--- Utils ----------------------------------------------------------------------

-- | Display an exception with its context
displayExceptionWithContext :: SomeException -> String
displayExceptionWithContext ex = do
  case displayExceptionContext (someExceptionContext ex) of
    "" -> displayException ex
    cx -> displayException ex ++ "\n\n" ++ cx
