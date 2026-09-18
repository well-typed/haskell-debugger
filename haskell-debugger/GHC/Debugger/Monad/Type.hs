{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NondecreasingIndentation #-}

module GHC.Debugger.Monad.Type where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Version (showVersion)
import Prelude hiding (mod)
#ifdef MIN_VERSION_unix
#endif
import qualified Data.List as L

import GHC
import GHC.Driver.DynFlags as GHC
import GHC.Types.Error
import GHC.Unit.State
import GHC.Unit.Types
import qualified GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC

import GHC.Debugger.Interface.Messages
import GHC.Debugger.Runtime.Compile.Cache
import qualified GHC.Debugger.Data.BreakpointMap as BM
import qualified GHC.Debugger.Data.ThreadMap     as TM

import Colog.Core as Logger

import {-# SOURCE #-} GHC.Debugger.Runtime.Instances.Discover (RuntimeInstancesCache, emptyRuntimeInstancesCache)
import GHC.Debugger.Utils.Orphans () -- bring orphan instances to everything which uses `Debugger`
import GHC.Debugger.Debuggee
import GHCi.RemoteTypes

-- | A debugger action.
newtype Debugger a = Debugger { unDebugger :: ReaderT DebuggerState GHC.Ghc a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadThrow, MonadCatch, MonadMask
           , GHC.HasDynFlags, MonadReader DebuggerState )

data BreakpointInfo = BreakpointInfo
  { bpInfoStatus :: !BreakpointStatus
  , bpInfoKind   :: !BreakpointKind
  , bpInfoAction :: !BreakpointAction
  }
  deriving (Eq,Show)

-- | State required to run the debugger.
--
-- - Keep track of active breakpoints to easily unset them all.
data DebuggerState = DebuggerState
      { activeBreakpoints :: IORef (BM.BreakpointMap BreakpointInfo)
        -- ^ Maps a 'InternalBreakpointId' in Trie representation (map of Module to map of Int) to the
        -- 'BreakpointStatus' it was activated with.

      , rtinstancesCache  :: IORef RuntimeInstancesCache
      -- ^ RuntimeInstancesCache

      , threadMap         :: IORef (TM.ThreadMap (ForeignRef ThreadId))
      -- ^ 'ThreadMap' for threads spawned by the debuggee

      , threadResumeMap   :: IORef (TM.ThreadMap Resume)
      -- ^ If a thread is currently stopped on a breakpoint, this map will
      -- contain its resume context (from which we can resume the thread)

      , compCache         :: IORef CompCache
      -- ^ Cache loaded and compiled expressions.

      , hsDbgViewUnitId   :: Maybe UnitId
      -- ^ The unit-id of the companion @haskell-debugger-view@ unit, used for
      -- user-defined and built-in custom debug visualisations of values (e.g.
      -- for Strings or IntMap).
      --
      -- If the user depends on @haskell-debugger-view@ in its transitive
      -- closure, then we should use that exact unit which was solved by Cabal.
      -- The built-in instances and additional instances be available for the
      -- 'DebugView' class found in that unit. We can find the exact unit of
      -- the module by looking for @haskell-debugger-view@ in the module graph.
      --
      -- If the user does not depend on @haskell-debugger-view@ in any way,
      -- then we create our own unit and try to load the
      -- @haskell-debugger-view@ modules directly into it. As long as loading
      -- succeeds, the 'DebugView' class from this custom unit can be used to
      -- find the built-in instances for types like @'String'@
      --
      -- If the user explicitly disabled custom views, use @Nothing@.

      , dbgLogger :: LogAction Debugger DebuggerLog
      -- ^ See Note [Debugger, debuggee, and DAP logs]
      }

instance GHC.HasLogger Debugger where
  getLogger = liftGhc GHC.getLogger

instance GHC.GhcMonad Debugger where
  getSession = liftGhc GHC.getSession
  setSession s = liftGhc $ GHC.setSession s

-- | Enabling/Disabling a breakpoint
data BreakpointStatus
      -- | Breakpoint is disabled
      --
      -- Note: this must be the first constructor s.t.
      --  @BreakpointDisabled < {BreakpointEnabled, BreakpointAfterCount}@
      = BreakpointDisabled
      -- | Breakpoint is enabled
      | BreakpointEnabled
      -- | Breakpoint is disabled the first N times and enabled afterwards
      | BreakpointAfterCount Int
      -- | Breakpoint is enabled when condition evaluates to true
      | BreakpointWhenCond String
      -- | Breakpoint is disabled the first N times the condition evaluates to
      -- true and enabled in the next time it is true
      | BreakpointAfterCountCond Int String
      deriving (Eq, Ord, Show)

instance Outputable BreakpointStatus where ppr = text . show

-- | What to do when a breakpoint is enabled
data BreakpointAction
      -- | Evaluation is stopped, typical behaviour
      = BreakpointStop
      {- | A log message is printed and then evaluation resumes.
        The @String@ is an expression that takes care of interpolation and printing the log message.
      -}
      | BreakpointLogAndResume String
      deriving (Eq, Ord, Show)

instance Outputable BreakpointAction where ppr = text . show

--------------------------------------------------------------------------------
-- Runner Types
--------------------------------------------------------------------------------

-- | Additional settings configuring the debugger
data RunDebuggerSettings = RunDebuggerSettings
      { supportsANSIStyling :: Bool
      , supportsANSIHyperlinks :: Bool
      , interpreterSettings :: InterpreterSettings
      }

type DebugSession m a
  =  FilePath -- ^ project root dir
  -> [String] -- ^ extra ghc args
  -> m ()   -- ^ action to load debugee home units
  -> Ghc a

type DebugRunner m a = DebugSession m a -> IO a

data ProjectDebugSpec = ProjectDebugSpec
      { rootDir :: FilePath
      -- ^ Project root directory
      , componentDir :: FilePath
      -- ^ Root dir of the loaded 'ComponentOptions'.
      -- Important for multi-package cabal projects, as packages are not in the
      -- root of the cradle, but in some sub-directory.
      , libdir :: FilePath
        -- ^ The libdir (given with -B as an arg)
      , units :: [String]
        -- ^ The list of units included in the invocation
      , ghcInvocation :: [String]
      -- ^ The full ghc invocation (as constructed by hie-bios flags)
      , absEntryFile :: FilePath
      -- ^ Path to the main function
      , extraGhcArgs :: [String]
      }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Initialize a 'DebuggerState'
initialDebuggerState :: LogAction Debugger DebuggerLog -> Maybe UnitId -> GHC.Ghc DebuggerState
initialDebuggerState l hsDbgViewUid =
  DebuggerState <$> liftIO (newIORef BM.empty)
                <*> liftIO (newIORef emptyRuntimeInstancesCache)
                <*> liftIO (newIORef TM.emptyThreadMap)
                <*> liftIO (newIORef TM.emptyThreadMap)
                <*> liftIO (newIORef emptyCompCache)
                <*> pure hsDbgViewUid
                <*> pure l

-- | Fetch the @haskell-debugger-view@ unit-id from the environment.
-- @Nothing@ means custom debugger views are disabled.
getHsDebuggerViewUid :: Debugger (Maybe UnitId)
getHsDebuggerViewUid = asks hsDbgViewUnitId

-- | Lift a 'Ghc' action into a 'Debugger' one.
liftGhc :: GHC.Ghc a -> Debugger a
liftGhc = Debugger . ReaderT . const

data DebuggerFailedToLoad = DebuggerFailedToLoad
instance Exception DebuggerFailedToLoad
instance Show DebuggerFailedToLoad where
  show DebuggerFailedToLoad = "Failed to compile and load user project."

data UnsupportedHsDbgViewVersion = UnsupportedHsDbgViewVersion
  { supportedVersions :: [ (Version, Version) ]
  , actualVersion :: Version
  }
instance Exception UnsupportedHsDbgViewVersion
instance Show UnsupportedHsDbgViewVersion where
  show (UnsupportedHsDbgViewVersion supported actual) =
    "Cannot use unsupported haskell-debugger-view version found in the transitive closure: " ++ showVersion actual ++
    " (supported: " ++ L.intercalate ", " (map (\(l,h) -> showVersion l ++ " <= && < " ++ showVersion h) supported) ++ ")"

data NonFatalException = NonFatalException { userMessage :: String, debugMessage :: String }
  deriving Show

instance Exception NonFatalException

expectRight :: Exception e => Either e a -> Debugger a
expectRight s = case s of
  Left e -> do
    logSDoc Logger.Error (text $ displayException e)
    liftIO $ throwIO $ NonFatalException { userMessage = displayException e, debugMessage = displayExceptionWithInfo $ toException e }
  Right a -> do
    pure a

logSDoc :: Logger.Severity -> SDoc -> Debugger ()
logSDoc sev doc = do
  dflags <- getDynFlags
  l <- asks dbgLogger
  l <& DebuggerLog sev (LogSDoc dflags doc)
