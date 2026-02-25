{-# LANGUAGE BlockArguments, OverloadedStrings, DerivingStrategies #-}
-- | Run the proxy mode, which forwards stdin/stdout to/from the DAP server and
-- is displayed in a terminal in the DAP client using 'runInTerminal'
module Development.Debug.Adapter.Proxy
  ( serverSideHdbProxy
  , runInTerminalHdbProxy
  ) where

import DAP

import Control.Concurrent.Async
import System.IO
import System.Exit (exitSuccess)
import System.Environment
import System.FilePath
import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import GHC.Conc.Sync (labelThread)
import qualified Data.List.NonEmpty as NE

import qualified Data.Text as T
import Network.Socket hiding (Debug)
import Network.Run.TCP
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as H

import Colog.Core
import Development.Debug.Adapter

-- | Fork a new thread to run the server-side of the proxy.
--
-- 1. To setup:
-- Ask the DAP client to launch a process running @hdb proxy --port <port>@
-- by sending a 'runInTerminal' DAP reverse request. This is done outside of
-- this function by signaling the given MVar (this is the case because we cannot use `network` with `DebugAdaptor`
--
-- 2. In a loop,
-- 2.1 Read stdin from the socket and push it to a Chan
-- 2.1 Read from a stdout Chan and write to the socket
serverSideHdbProxy :: LogAction IO (WithSeverity T.Text)
                   -> MVar ()
                   -> DebugAdaptor ()
serverSideHdbProxy l client_conn_signal = do
  DAS { syncProxyIn = dbIn
      , syncProxyOut = dbOut
      , syncProxyErr = dbErr } <- getDebugSession

  sock <- liftIO $ do
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
    addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
    -- Bind on "0" to let the OS pick a free port
    openTCPServerSocket addr

  port <- liftIO $ socketPort sock

  fwd_thr <- liftIO $ async $ ignoreIOException $ do
    myThreadId >>= \tid -> labelThread tid "Debug/Adapter/Proxy: TCP Server"
    runTCPServerWithSocket sock $ \scket -> do

      infoMsg (T.pack $ "Connected to client on port " ++ show port ++ "...!")
      putMVar client_conn_signal () -- signal ready (see #95)

      race_
        (race_
          (-- Read stdout from chan and write to socket
           ignoreIOException $ do
             tid <- myThreadId
             labelThread tid "Debug/Adapter/Proxy: Forward stdout"
             forever $ do
               bs <- readChan dbOut
               debugMsg (T.pack $ "Writing to socket: " ++ BS8.unpack bs)
               NBS.sendAll scket bs)
          (-- Read stderr from chan and write to socket
           ignoreIOException $ do
             tid <- myThreadId
             labelThread tid "Debug/Adapter/Proxy: Forward stderr"
             forever $ do
               bs <- readChan dbErr
               debugMsg (T.pack $ "Writing to socket (from stderr): " ++ BS8.unpack bs)
               NBS.sendAll scket bs))
        (-- Read stdin from socket and write to chan
         let
          loop = do
            bs <- NBS.recv scket 4096
            if BS8.null bs
              then do
                debugMsg (T.pack "Connection to client was closed.")
                close scket
              else do
                debugMsg (T.pack $ "Read from socket: " ++ BS8.unpack bs)
                writeChan dbIn bs >> loop
          in ignoreIOException loop)

  liftIO $ link fwd_thr
  sendRunProxyInTerminal port

  where
    ignoreIOException a = catch a $ \(e::IOException) ->
      infoMsg (T.pack $ "Ignoring connection broken to proxy client: " ++ show e)
    debugMsg msg = l <& WithSeverity msg Debug
    infoMsg msg  = l <& WithSeverity msg Info

-- | The proxy code running on the terminal in which the @hdb proxy@ process is launched.
--
-- This client-side proxy is responsible for
-- 1. Connecting to the given proxy-server port
-- 2. Forwarding stdin to the port it is connected to
-- 3. Read from the network the output and write it to stdout
runInTerminalHdbProxy :: LogAction IO (WithSeverity T.Text) -> Int -> IO ()
runInTerminalHdbProxy l port = do
  l <& WithSeverity (T.pack $ "Running in terminal on port " ++ show port ++ "...!") Info
  hSetBuffering stdin LineBuffering

  dbg_inv <- lookupEnv "DEBUGGEE_INVOCATION"
  case dbg_inv of
    Nothing  -> pure ()
    Just inv ->
      putStrLn $ "Running the debugger input/output proxy for the following debuggee execution:\n\n\n    " ++ inv ++ "\n\n"

  catch (
    runTCPClient "127.0.0.1" (show port) $ \sock -> do
      -- Forward stdin to sock
      race_
        (catch (forever $ do
          str <- BS8.hGetLine stdin
          NBS.sendAll sock (str <> BS8.pack "\n")
          ) $ \(_e::IOException) -> return ()) -- connection dropped, just exit.

        (-- Forward stdout from sock
        catch (forever $ do
          msg <- NBS.recv sock 4096
          if BS8.null msg
            then do
              l <& WithSeverity (T.pack "Exiting...") Info
              close sock
              exitSuccess
            else BS8.hPut stdout msg >> hFlush stdout
          ) $ \(_e::IOException) -> return ()) -- connection dropped, just exit.

    ) $ \(_e::IOException) -> do
      hPutStrLn stderr "Failed to connect to debugger server proxy -- did the debuggee compile and start running successfully?"

-- | Send a 'runInTerminal' reverse request to the DAP client
-- with the @hdb proxy@ invocation
sendRunProxyInTerminal :: PortNumber -> DebugAdaptor ()
sendRunProxyInTerminal port = do
  DAS { entryFile
      , entryPoint
      , entryArgs
      , projectRoot } <- getDebugSession
  let debuggee_inv = T.pack $ makeRelative projectRoot entryFile ++ ":" ++ entryPoint ++
                              (if null entryArgs then "" else " ") ++ unwords entryArgs
  sendRunInTerminalReverseRequest
    RunInTerminalRequestArguments
      { runInTerminalRequestArgumentsKind = Just RunInTerminalRequestArgumentsKindIntegrated
      , runInTerminalRequestArgumentsTitle = Just debuggee_inv
      , runInTerminalRequestArgumentsCwd = ""
      , runInTerminalRequestArgumentsArgs = ["hdb", "proxy", "--port", T.pack (show port)]
      , runInTerminalRequestArgumentsEnv = Just (H.singleton "DEBUGGEE_INVOCATION" debuggee_inv)
      , runInTerminalRequestArgumentsArgsCanBeInterpretedByShell = False
      }
