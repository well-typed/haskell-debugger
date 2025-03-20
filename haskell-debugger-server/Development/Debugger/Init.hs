{-# LANGUAGE OverloadedStrings, RecordWildCards, DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
module Development.Debugger.Init where

import qualified Data.Text as T
import Control.Monad.IO.Class
import Data.Functor
import System.IO
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import GHC.Generics

import Development.Debugger.Flags
import Development.Debugger.Adaptor

import qualified Debugger
import qualified Debugger.Monad as Debugger
import Debugger.Interface.Messages hiding (Command, Response)
import qualified Debugger.Interface.Messages as D (Command, Response)
import System.Directory
import System.FilePath

import DAP

--------------------------------------------------------------------------------
-- * Client
--------------------------------------------------------------------------------

-- | Client arguments are custom for launch
data LaunchArgs
  = LaunchArgs
  { __sessionId :: Maybe String
    -- ^ SessionID, set by VSCode client
  , projectRoot :: FilePath
    -- ^ Absolute path to the project root
  , entryFile :: FilePath
    -- ^ The file with the entry point e.g. @app/Main.hs@
  , entryPoint :: String
    -- ^ Either @main@ or a function name
  , entryArgs :: [String]
    -- ^ The arguments to either set as environment arguments when @entryPoint = "main"@
    -- or function arguments otherwise.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass FromJSON

--------------------------------------------------------------------------------
-- * Launch Debugger
--------------------------------------------------------------------------------

-- | Initialize debugger
--
-- todo items:
-- [ ] Consider Events (Async) vs Responses (Sync)
-- [ ] Consider exception handling leading to termination
-- [ ] Keep map from fresh breakpoint ids to breakpoint internally
initDebugger :: LaunchArgs -> DebugAdaptor ()
initDebugger LaunchArgs{__sessionId, projectRoot, entryFile, entryPoint, entryArgs} = do

  -- This is a unfortunate because the current directory is global to the
  -- process. But cabal and ghc require the current directory to be the root of
  -- the project so we set it here.
  --
  -- TODO: This means we don't support debugging using the same server two
  -- different projects on separate workspaces.
  --
  -- PRIORITY:IMPORTANT
  liftIO $ setCurrentDirectory projectRoot

  syncRequests  <- liftIO newEmptyMVar
  syncResponses <- liftIO newEmptyMVar
  flags <- liftIO $ hieBiosFlags (projectRoot </> entryFile)

  let nextFreshBreakpointId = 0
      breakpointMap = mempty

  registerNewDebugSession (maybe "debug-session" T.pack __sessionId) DAS{..}
    [ \_withAdaptor -> debuggerThread flags syncRequests syncResponses

    -- , if we make the debugger emit events (rather than always replying synchronously),
    -- we will listen for them here to forward them to the client.
    ]

-- | The main debugger thread reads commands and writes responses synchronously
-- to the given synchronization variables in a new 'Debugger' session.
debuggerThread :: HieBiosFlags -> MVar D.Command -> MVar D.Response -> IO ()
debuggerThread HieBiosFlags{..} requests replies =
  Debugger.runDebugger libdir units ghcInvocation $
    forever $ do
      req <- liftIO $ takeMVar requests
      resp <- (Debugger.execute req <&> Right)
                `catch` \(e :: SomeException) -> pure (Left (displayException e))
      either bad reply resp
  where
    reply = liftIO . putMVar replies
    bad m = liftIO $ do
      hPutStrLn stderr m
      putMVar replies (Aborted m)

