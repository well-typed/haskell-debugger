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
module Development.Debugger.Output
  ( console, important, stdout, stderr )
  where

import Data.Text (Text)
import qualified Data.Text as T
import DAP (sendOutputEvent, defaultOutputEvent, OutputEvent(..), OutputEventCategory(..))
import Development.Debugger.Adaptor

-- | Show the output in the client's default message UI, e.g. a 'debug
-- console'. This category should only be used for informational output from
-- the debugger (as opposed to the debuggee).
console :: Text -> DebugAdaptorX r
console = send Console

-- | 'important': A hint for the client to show the output in the client's UI
-- for important and highly visible information, e.g. as a popup notification.
-- This category should only be used for important messages from the debugger
-- (as opposed to the debuggee).
important :: Text -> DebugAdaptorX r
important = send Important

-- | Show the output as normal program output from the debuggee.
stdout :: Text -> DebugAdaptorX r
stdout = send Stdout

-- | Show the output as error program output from the debuggee.
stderr :: Text -> DebugAdaptorX r
stderr = send Stderr

-- | Generic send output event given the category
send :: OutputEventCategory -> Text -> DebugAdaptorX r
send cat txt = do
  sendOutputEvent defaultOutputEvent
    { outputEventCategory = Just cat
    , outputEventOutput = txt <> T.pack "\n"
    }
