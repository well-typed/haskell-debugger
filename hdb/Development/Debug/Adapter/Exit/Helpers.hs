module Development.Debug.Adapter.Exit.Helpers where

import DAP
import Data.Function
import System.IO
import Control.Monad
import Control.Monad.IO.Class
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
  has_data <- hReady final_handle & liftIO
  when has_data $ do
      -- get all pending output from GHC
      c <- T.hGetContents final_handle & liftIO
      Output.neutral c
  terminateWithError msg

-- | Abruptly terminate a session in the middle of a Request/Response cycle by
-- sending a Terminated event (meaning the debug session is over), destroying
-- the debug session threads, and replying to the response with 'ErrorResponse'
terminateWithError :: String -> DebugAdaptor ()
terminateWithError msg = do
  Output.important (T.pack msg)
  destroyDebugSession
  sendTerminatedEvent (TerminatedEvent False)
  sendError (ErrorMessage (T.pack msg)) Nothing
