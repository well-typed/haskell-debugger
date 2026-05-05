-- | Meant to be imported qualified:
--
-- @
-- import qualified Development.Debugger.Output as Output
--
-- Output.console "Category used for informational output from debugger, not debuggee"
-- Output.stdout  "Standard out of debuggee"
-- Output.stderr  "Standard err of debuggee"
-- ...
-- @
--
-- TODO:
--  [ ] ANSI Styling of output console messages?
--
module Development.Debug.Adapter.Output
  ( neutral, console, important, stdout, stderr )
  where

import Data.Text (Text)
import qualified Data.Text as T
import DAP (sendOutputEvent, defaultOutputEvent, OutputEvent(..), OutputEventCategory(..))
import Development.Debug.Adapter

-- | Default 'OutputEvent' without an explicit category.
neutral :: Text -> DebugAdaptorX r
neutral = send Nothing

-- | Show the output in the client's default message UI, e.g. a 'debug
-- console'. This category should only be used for informational output from
-- the debugger (as opposed to the debuggee).
console :: Text -> DebugAdaptorX r
console = send $ Just OutputEventCategoryConsole

-- | 'important': A hint for the client to show the output in the client's UI
-- for important and highly visible information, e.g. as a popup notification.
-- This category should only be used for important messages from the debugger
-- (as opposed to the debuggee).
important :: Text -> DebugAdaptorX r
important = send $ Just OutputEventCategoryImportant

-- | Show the output as normal program output from the debuggee.
stdout :: Text -> DebugAdaptorX r
stdout = send $ Just OutputEventCategoryStdout

-- | Show the output as error program output from the debuggee.
stderr :: Text -> DebugAdaptorX r
stderr = send $ Just OutputEventCategoryStderr

-- | Generic send output event given the category
send :: Maybe OutputEventCategory -> Text -> DebugAdaptorX r
send cat txt = do
  sendOutputEvent defaultOutputEvent
    { outputEventCategory = cat
    , outputEventOutput = txt <> T.pack "\n"
    }
