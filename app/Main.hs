{-

Main launches 3 lightweight threads.
- One thread listens to requests and writes them to a `Chan` (C1=requests)
- Another reads from `Chan` (C2=replies) and sends replies
- The worker thread reads requests from (C1), executes (e.g. by interpreting the debuggee program until a break), and writes responses to (C2)

┌────────────────────────────────────┐          
│Main                                │          
└┬────────────────────┬─────────────┬┘          
┌▽──────────────────┐┌▽───────────┐┌▽──────────┐
│Listen for requests││Send replies││GHC session│
└───────────────────┘└────────────┘└───────────┘

-}
module Main where

import Control.Concurrent
import Control.Monad

import Debugger.Interface

main :: IO ()
main = do
  requests <- newChan
  replies  <- newChan

  _ <- forkIO $ receiver requests
  _ <- forkIO $ sender replies
  _ <- forkIO $ debugger requests replies

  return ()

-- | The main worker. Runs a GHC session which executes 'Request's received from
-- the given @'Chan' 'Request'@ and writes 'Response's to the @'Chan' 'Response'@ channel.
debugger :: Chan Request -> Chan Response -> IO ()
debugger requests replies = do
  req <- readChan requests
  case req of
    SetBreakpoint bp -> _
    DelBreakpoint bp -> _
    GetStacktrace -> _
    GetVariables -> _
    GetSource -> _
    DoEval -> _
    DoContinue -> _
    DoStepLocal -> _
    DoSingleStep -> _
