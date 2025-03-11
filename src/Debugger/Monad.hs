{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, NamedFieldPuns, TupleSections, LambdaCase #-}
module Debugger.Monad where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import Control.Monad.Catch

import System.Exit

import qualified GHC
import qualified GHC.Driver.DynFlags as GHC
import qualified GHC.Driver.Phases as GHC
import qualified GHC.Driver.Pipeline as GHC
import qualified GHC.Driver.Config.Logger as GHC
import qualified GHC.Driver.Session.Units as GHC
import qualified GHC.Driver.Session.Mode as GHC
import qualified GHC.Driver.Session.Lint as GHC
import qualified GHC.Utils.Monad as GHC
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Types.Unique.Supply as GHC
import qualified GHC.Runtime.Loader as GHC
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List

-- | A debugger action
newtype Debugger a = Debugger { unDebugger :: GHC.Ghc a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a 'Debugger' action on a session constructed from a given GHC invocation.
runDebugger :: Maybe FilePath -- ^ The libdir (given with -B as an arg)
            -> [String]       -- ^ The list of units included in the invocation
            -> [String]       -- ^ The full ghc invocation (as constructed by hie-bios flags)
            -> Debugger a     -- ^ 'Debugger' action to run on the session constructed from this invocation
            -> IO a
runDebugger libdir units ghcInvocation' (Debugger action) = do
  let ghcInvocation = filter (\case ('-':'B':_) -> False; _ -> True) ghcInvocation'

  GHC.runGhc libdir $ do

    dflags0 <- GHC.getSessionDynFlags

    let dflags1 = dflags0
          { GHC.ghcMode = GHC.CompManager
          , GHC.backend = GHC.interpreterBackend
          , GHC.ghcLink = GHC.LinkInMemory
          , GHC.verbosity = 1
          }
          -- Default GHCi settings
          `GHC.gopt_set` GHC.Opt_ImplicitImportQualified
          `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
          `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
          `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
          `GHC.gopt_set` GHC.Opt_InsertBreakpoints

    logger1 <- GHC.getLogger
    let logger2 = GHC.setLogFlags logger1 (GHC.initLogFlags dflags1)

          -- The rest of the arguments are "dynamic"
          -- Leftover ones are presumably files
    (dflags4, fileish_args, _dynamicFlagWarnings) <-
        GHC.parseDynamicFlags logger2 dflags1 (map (GHC.mkGeneralLocated "on ghc-debugger command arg") ghcInvocation)

    let (dflags5, srcs, objs) = GHC.parseTargetFiles dflags4 (map GHC.unLoc fileish_args)

    -- we've finished manipulating the DynFlags, update the session
    _ <- GHC.setSessionDynFlags dflags5

    dflags6 <- GHC.getSessionDynFlags

    -- Should this be done in GHC=
    liftIO $ GHC.initUniqSupply (GHC.initialUnique dflags6) (GHC.uniqueIncrement dflags6)

    -- Initialise plugins here because the plugin author might already expect this
    -- subsequent call to `getLogger` to be affected by a plugin.
    GHC.initializeSessionPlugins
    hsc_env <- GHC.getSession
    logger <- GHC.getLogger

    ---------------- Final sanity checking -----------
    liftIO $ GHC.checkOptions GHC.DoInteractive dflags6 srcs objs units
    
    hs_srcs <- case NE.nonEmpty units of
      Just ne_units -> do
        GHC.initMulti ne_units
      Nothing -> do
        case srcs of
          [] -> return []
          _  -> do
            -- ROMES: think about where this initMake should go in GHC or if it should really be copied over.
            let (hs_srcs, non_hs_srcs) = List.partition GHC.isHaskellishTarget srcs

            -- if we have no haskell sources from which to do a dependency
            -- analysis, then just do one-shot compilation and/or linking.
            -- This means that "ghc Foo.o Bar.o -o baz" links the program as
            -- we expect.
            if (null hs_srcs)
               then liftIO (GHC.oneShot hsc_env GHC.NoStop srcs) >> return []
               else do
                 o_files <- GHC.mapMaybeM (\x -> liftIO $ GHC.compileFile hsc_env GHC.NoStop x) non_hs_srcs
                 dflags7 <- GHC.getSessionDynFlags
                 let dflags' = dflags7 { GHC.ldInputs = map (GHC.FileOption "") o_files ++ GHC.ldInputs dflags7 }
                 _ <- GHC.setSessionDynFlags dflags'
                 return $ map (uncurry (,Nothing,)) hs_srcs

    targets' <- mapM (\(src, uid, phase) -> GHC.guessTarget src uid phase) hs_srcs
    GHC.setTargets targets'
    ok_flag <- GHC.load GHC.LoadAllTargets
    when (GHC.failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

    -- TODO: initLoaderState?

    action
