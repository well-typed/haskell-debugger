{-# LANGUAGE BlockArguments, OverloadedStrings, DerivingStrategies, OrPatterns #-}
{-# LANGUAGE NondecreasingIndentation #-}
-- | Run the proxy mode, which forwards stdin/stdout to/from the DAP server and
-- is displayed in a terminal in the DAP client using 'runInTerminal'.
--
-- Note: the proxy program is only launched when 'runInTerminal' is supported
-- and we're using the internal interpreter (--internal-interpreter).
--
-- If the external interpreter is being used (the default), we launch the
-- external interpreter directly with 'runInTerminal' and don't need the proxy
-- at all.
module Development.Debug.Adapter.Proxy
  ( mkServerSideHdbProxy
  , runInTerminalHdbProxy
  , sendRunProxyInTerminal
  , openSocketAvailablePort
  ) where

#if !MIN_VERSION_ghc(9,15,0)
-- no longer needs to be imported in 9.15
import GHC.Conc.Sync (labelThread)
#endif

import DAP

import Control.Concurrent.Async
import System.IO
import System.Exit (exitSuccess)
import System.Environment
import System.FilePath
import Control.Exception.Base
import Control.Monad
import Control.Concurrent
import qualified Data.List.NonEmpty as NE

import qualified Data.Text as T
import Network.Socket hiding (Debug)
import Network.Run.TCP
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as H

import Colog.Core
import Development.Debug.Adapter
import qualified Control.Exception as E
import GHC.Debugger.Interface.Messages (unAbs)
import GHC.Debugger.Utils (silenceEOF)

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
mkServerSideHdbProxy :: LogAction IO (WithSeverity T.Text)
                   -> Chan BS8.ByteString
                   -> Chan BS8.ByteString
                   -> Chan BS8.ByteString
                   -> MVar ()
                   -> IO (PortNumber, IO ())
mkServerSideHdbProxy l dbIn dbOut dbErr client_conn_signal =
  bracketOnError openSocketAvailablePort close $ \ sock -> do

  port <- socketPort sock

  return $ (port,) $ do
   ignoreIOException $ do
    myThreadId >>= \tid -> labelThread tid "Debug/Adapter/Proxy: TCP Server"
    runTCPServerWithSocket' sock $ \scket -> do

      -- The proxy is machine-local, so we don't need the delays of TCP.
      setSocketOption scket NoDelay 1

      infoMsg (T.pack $ "Connected to client on port " ++ show port ++ "...!")
      putMVar client_conn_signal () -- signal ready (see #95)

      -- TODO: we use race here and concurrently on the client side.
      -- race = cancel others when one ends
      -- concurrently = wait for all to end
      --
      --
      race_
        (race_
          (-- Read stdout from chan and write to socket
           ignoreIOException $ do
             labelMe "Debug/Adapter/Proxy: Forward stdout"
             forever $ mask_ $ do
               bs <- readChan dbOut
               debugMsg (T.pack $ "Writing to socket: " ++ BS8.unpack bs)
               NBS.sendAll scket bs)
          (-- Read stderr from chan and write to socket
           ignoreIOException $ do
             labelMe "Debug/Adapter/Proxy: Forward stderr"
             forever $ mask_ $ do
               bs <- readChan dbErr
               debugMsg (T.pack $ "Writing to socket (from stderr): " ++ BS8.unpack bs)
               NBS.sendAll scket bs))
        (-- Read stdin from socket and write to chan
         let
          loop = join $ mask_ $ do
            bs <- NBS.recv scket 4096
            if BS8.null bs
              then do
                debugMsg (T.pack "Connection to client was closed.")
                -- Let runTCPServer do it.
                -- close scket
                pure $ pure ()
              else do
                debugMsg (T.pack $ "Read from socket: " ++ BS8.unpack bs)
                writeChan dbIn bs
                pure loop
          in ignoreIOException $ do
              labelMe "Debug/Adapter/Proxy: Read stdin"
              loop)

  where
    -- TODO: more specific catching?
    ignoreIOException a = catch a $ \(e::IOException) ->
      infoMsg (T.pack $ "Ignoring connection broken to proxy client: " ++ show e)
    debugMsg msg = l <& WithSeverity msg Debug
    infoMsg msg  = l <& WithSeverity msg Info

-- | A version of @runTCPServerWithSocket@ that kills the forked connection
-- handlers when killed.
runTCPServerWithSocket' :: Socket -> (Socket -> IO a1) -> IO a2
runTCPServerWithSocket' sock server = do
  let
    gClose conn = gracefulClose conn 5000
    serverLoop = E.bracketOnError (accept sock) (gClose . fst) $
      \(conn, _peer) ->
        -- TODO: what is mask_ achieving here? does it mean we can drop E.bracketOnError?
        mask_ $ withAsyncWithUnmask
          (\ unmask ->
             unmask (labelMe "TCP Server handler" >> server conn)
            `finally` gClose conn)
          (const serverLoop)
  serverLoop

-- | Label the running thread
labelMe :: String -> IO ()
labelMe name = do
    tid <- myThreadId
    labelThread tid name

-- | Open a socket on an available port
openSocketAvailablePort :: IO Socket
openSocketAvailablePort = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV] ++ [AI_PASSIVE]  -- For wildcard IP (0.0.0.0 or ::)
                            , addrSocketType = Stream

                        , addrFamily = AF_UNSPEC    -- Allow IPv4 or IPv6
                        }
  addr <- NE.head <$> getAddrInfo (Just hints) Nothing (Just "0")
  openTCPServerSocketFixed addr
  where
    openTCPServerSocketFixed addr = do
      bracketOnError (openSocket addr) Network.Socket.close $ \ sock -> do
      setSocketOption sock ReuseAddr 1
      -- openTCPServerSocket from network-run includes this commented out snippet which causes test failures ("runInTerminal: proxy forwards stdin correctly") on macOS.
      -- #if !defined(openbsd_HOST_OS)
      --   when (addrFamily addr == AF_INET6) $ setSocketOption sock IPv6Only 1
      -- #endif
      mapM_ (uncurry $ setSockOptValue sock) []
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock maxListenQueue
      return sock

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
  let settings = defaultSettings { settingsOpenClientSocket = openClientSocketWithOptions [(NoDelay,1)] }
  catch (
    runTCPClientWithSettings settings "127.0.0.1" (show port) $ \sock -> do
      -- Forward stdin to sock
      concurrently_
        (silenceEOF stdin $ -- stdin closed, just exit.
                            -- TODO: what about sendAll exceptions? WAS: catch IOException and dropping it, with comment "connection dropped, just exit".
          -- TODO: reuse forwarding Thread
          forever $ mask_ $ do
            str <- BS8.hGetLine stdin
            NBS.sendAll sock (str <> BS8.pack "\n"))

        (-- Forward stdout from sock
        catch (forever $ do
          msg <- NBS.recv sock 4096
          if BS8.null msg
            then do
              l <& WithSeverity (T.pack "Exiting...") Info
              -- Let runTCPClient do it gracefully.
              -- close sock
              exitSuccess
            else BS8.hPut stdout msg >> hFlush stdout
          ) $ \(_e::IOException) -> return ()) -- connection dropped, just exit.
              -- TODO: can we be more specific?
    ) $ \(_e::IOException) -> do
      hPutStrLn stderr "Failed to connect to debugger server proxy -- did the debuggee compile and start running successfully?"

-- | Send a 'runInTerminal' reverse request to the DAP client
-- with the @hdb proxy@ invocation
sendRunProxyInTerminal :: FilePath -> PortNumber -> DebugAdaptor ()
sendRunProxyInTerminal hdbProg port = do
  DAS { entryFile
      , entryPoint
      , entryArgs
      , projectRoot } <- getDebugSession
  let debuggee_inv = T.pack $ makeRelative (unAbs projectRoot) (unAbs entryFile) ++ ":" ++ entryPoint ++
                              (if null entryArgs then "" else " ") ++ unwords entryArgs
  sendRunInTerminalReverseRequest
    RunInTerminalRequestArguments
      { runInTerminalRequestArgumentsKind = Just RunInTerminalRequestArgumentsKindIntegrated
      , runInTerminalRequestArgumentsTitle = Just debuggee_inv
      , runInTerminalRequestArgumentsCwd = ""
      , runInTerminalRequestArgumentsArgs = [T.pack hdbProg, "proxy", "--port", T.pack (show port)]
      , runInTerminalRequestArgumentsEnv = Just (H.singleton "DEBUGGEE_INVOCATION" debuggee_inv)
      , runInTerminalRequestArgumentsArgsCanBeInterpretedByShell = False
      }
