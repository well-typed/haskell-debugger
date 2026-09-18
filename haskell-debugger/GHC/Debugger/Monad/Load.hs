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

module GHC.Debugger.Monad.Load
where

import Control.Exception
import Control.Monad
import Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Data.Function
import Data.Maybe
import qualified Data.Set as Set
import Data.Version (makeVersion)
import Prelude hiding (mod)
#ifdef MIN_VERSION_unix
#endif
import qualified Data.List as L

import GHC
import GHC.Data.StringBuffer
import GHC.Driver.Config.Diagnostic
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Env as GHC
import GHC.Driver.Monad
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Driver.Ppr
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Unit.Module.Graph
import GHC.Unit.State
import GHC.Unit.Types
import qualified GHC.Utils.Logger as GHC
import GHC.Utils.Outputable as GHC

import GHC.Debugger.Session
import GHC.Debugger.Session.Builtin

import Colog.Core as Logger

import GHC.Platform.Ways
import GHC.Debugger.Utils.Orphans () -- bring orphan instances to everything which uses `Debugger`
import GHC.Debugger.Debuggee
import GHC.Debugger.Monad.Type
import qualified GHC.Unit.Module.Graph as GHC
import GHC.Debugger.Interface.Messages (AbsFilePath, mkAbsolute, (/>))
import System.Directory (getCurrentDirectory)


-- | Throws exception when module fails to load.
loadInternal
  :: LogAction IO DebuggerLog
  -> Ways
  -> Ghc ()
loadInternal l buildWays = do
  let ghcLog = liftLogIO l

  dflags <- getDynFlags
  addInMemoryDebuggerInternalUnit (setDynFlagWays buildWays dflags)
  let uid = debuggerInternalUnitId
  successes <- loadInMemoryModules l uid modsToLoad
  forM_ (zip successes modsToLoad) $ \case
    (Failed,(modName,_)) -> do
      ghcLog <& DebuggerLog Logger.Debug
        (LogFailedToCompileBuiltinModule modName)
      liftIO $ fail "Failed to load DebuggerInternal Module"
    (Succeeded,_) ->
      return ()
  where
    modsToLoad =
      [(debuggerRuntimeInternalModName,debuggerRuntimeInternalContents)]

#if !MIN_VERSION_ghc(9,14,2)
data FailedToLoadFFIInspectModule = FailedToLoadFFIInspectModule
  deriving Show
instance Exception FailedToLoadFFIInspectModule

-- | Throws exception when module fails to load.
--   Needed for GHC.Debugger.Runtime.Interpreter.Legacy
loadFFIInspect
  :: LogAction IO DebuggerLog
  -> Ways
  -> Ghc ()
loadFFIInspect l buildWays = do
  let ghcLog = liftLogIO l

  dflags <- getDynFlags
  uid <- addInMemoryFFIInspectUnit [baseUnitId dflags] (setDynFlagWays buildWays dflags)

  successes <- loadInMemoryModules l uid modsToLoad
  forM_ (zip successes modsToLoad) $ \case
    (Failed,(modName,_)) -> do
      ghcLog <& DebuggerLog Logger.Debug
        (LogFailedToCompileBuiltinModule modName)
      liftIO $ throwIO FailedToLoadFFIInspectModule
    (Succeeded,_) ->
      return ()
  where
    modsToLoad =
      [(debuggerRuntimeFFIInspectModName,debuggerRuntimeFFIInspectContents)]
#endif

findOrLoadHaskellDebuggerView :: LogAction IO DebuggerLog
             -> Ways
             -> Ghc (UnitId, [ModuleName])
findOrLoadHaskellDebuggerView l buildWays = do
  let ghcLog = liftLogIO l
  hsc_env <- getSession

  -- Try to find or load the built-in classes from `haskell-debugger-view`
  findHsDebuggerViewUnitId >>= \case
    Nothing -> (hsDebuggerViewInMemoryUnitId,) <$> do
      -- Not imported by any module: no custom views. Therefore, the builtin
      -- ones haven't been loaded. In this case, we will load the package ourselves.

      -- Add the custom unit to the HUG
      let base_dep_uids = graphsUnits hsc_env
      addInMemoryHsDebuggerViewUnit base_dep_uids . setDynFlagWays buildWays =<< getDynFlags

      -- Load unit modules using in-memory contents.
      let
        -- Don't try to load instances whose packages are not even in the
        -- module graph.
        (instanceMods,skipped) = L.partition (\ (_modName,_modContent,pkgName) -> any ((pkgName `L.isPrefixOf`) . unitIdString) base_dep_uids)
            debuggerViewInstancesMods
        modsToLoad =
          (debuggerViewClassModName,debuggerViewClassContents)
          : [ (modName,modContent)
            | (modName, modContent, _pkgName) <- instanceMods]

      forM_ skipped $ \(modName,_,pkgName) ->
        ghcLog <& DebuggerLog Logger.Debug
          (LogSkippingViewModuleNoPkg modName pkgName (map unitIdString base_dep_uids))

      successes <- loadInMemoryModules l hsDebuggerViewInMemoryUnitId modsToLoad

      fmap catMaybes . forM (zip successes modsToLoad) $ \case
        (Failed,(modName,_)) -> do
          ghcLog <& DebuggerLog Logger.Debug
            (LogFailedToCompileBuiltinModule modName)
          return $ Nothing
        (Succeeded,(modName,_)) ->
          return $ Just modName

    Just uid -> do
      -- TODO: We assume for now that if you depended on
      -- @haskell-debugger-view@, then you also depend on all its transitive
      -- dependencies (containers, text, ...), thus can load all custom
      -- views. Hence all `debuggerViewBuiltinMods`. In the future, we
      -- may want to guard all dependencies behind cabal flags that the user
      -- can tweak when depending on `haskell-debugger-view`.
      return (uid, map fst debuggerViewBuiltinMods)



--------------------------------------------------------------------------------

-- | Run downsweep on the currently set targets (see @hsc_targets@)
doDownsweep :: GhcMonad m
            => Maybe ModuleGraph -- ^ Re-use existing module graph which was already summarised
            -> m ModuleGraph -- ^ Module graph constructed from current set targets
doDownsweep reuse_mg = do
  hsc_env <- getSession
  let msg = batchMultiMsg
  (errs_base, mod_graph) <- liftIO $
    downsweep
      hsc_env mkUnknownDiagnostic (Just msg)
      (maybe [] mgModSummaries reuse_mg)
#if MIN_VERSION_ghc(10,1,0)
      reuse_mg
#endif
      [] False
  when (not $ null errs_base) $ do
    -- Print the errors to the user, rather than just throwing. When using DAP,
    -- outputting to the logger the error is what displays it in the "Debug
    -- Console" rather than "Output" DAP log.
    logger <- getLogger
    dflags <- hsc_dflags <$> getSession
    let ghc_errs = fmap GhcDriverMessage (unionManyMessages errs_base)
    liftIO $ printMessages logger (initPrintConfig dflags) (initDiagOpts dflags) ghc_errs
#if MIN_VERSION_ghc(9,15,0)
    throwErrors (initSourceErrorContext dflags) ghc_errs
#else
    throwErrors ghc_errs
#endif
  return mod_graph

doLoad :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> ModuleGraph -> m SuccessFlag
doLoad if_cache how_much mg = do
  let msg = batchMultiMsg
  load' if_cache how_much mkUnknownDiagnostic (Just msg) mg


loadInMemoryModules ::
  LogAction IO DebuggerLog
  -> UnitId
  -> [(ModuleName,StringBuffer)] -> Ghc [SuccessFlag]
loadInMemoryModules l uid ts = do
  tgts <- forM ts $  \(modName,modContents) ->
    liftIO $ makeInMemoryTarget uid modName modContents
  GHC.setTargets tgts
  mod_graph <- hsc_mod_graph <$> GHC.getSession
  dvc_mod_graph <- doDownsweep (Just mod_graph)
  let new_mod_graph
#if MIN_VERSION_ghc(10,1,0)
        -- new API allows extending an existing graph.
        = dvc_mod_graph
#else
        = mkModuleGraph $ mg_mss dvc_mod_graph ++ mg_mss mod_graph
#endif
  modifySession $ GHC.setModuleGraph new_mod_graph

  restore_logger <- GHC.getLogger
  dflags <- getSessionDynFlags
  GHC.modifyLogger $
    -- Emit it all as Debug-level debugger logs
    GHC.pushLogHook $ const $ \_ _ _ sdoc ->
      l <& DebuggerLog Logger.Debug (LogSDoc dflags sdoc)

  -- Might not make sense to keep going if the first fails, but we expect all of
  -- them to succeed, and it's not that many more modules.
  s <- forM tgts $ \ tgt -> compileModuleWithDepsInHpt tgt >>= \case
        Nothing -> pure Succeeded
        Just e -> do
          liftLogIO l <& DebuggerLog Logger.Debug (LogSDoc dflags $ text (show e))
          pure Failed

  -- Restore logger
  GHC.modifyLogger $
    GHC.pushLogHook (const $ GHC.putLogMsg restore_logger)

  return s

--------------------------------------------------------------------------------
-- * Finding Debugger View
--------------------------------------------------------------------------------

-- | Try to find the @haskell-debugger-view@ unit-id in the transitive closure,
-- or, otherwise, return the a custom unit for which we'll load the
-- @haskell-debugger-view@ modules in it (essentially preparing an in-memory
-- version of the library to find the built-in instances in).
--
-- See also comment on the @'hsDbgViewUnitId'@ field of @'DebuggerState'@
findHsDebuggerViewUnitId :: GHC.Ghc (Maybe UnitId)
findHsDebuggerViewUnitId = do
  hsc_env <- getSession
  let unitState = hsc_units hsc_env

  -- Note: linear in the module graph but only happens once.
  let potential_units = graphsUnits hsc_env
  -- Note: the intermediate set is expected to be small (<= 2).
  let hskl_dbgr_vws = Set.toList . Set.fromList $
        [ uid
        | uid <- potential_units
        , let uid_s = unitIdString uid
        , "haskell-debugger-view" `L.isPrefixOf` uid_s
            || "hskll-dbggr-vw" `L.isPrefixOf` uid_s
            || "haskell-debug_" `L.isPrefixOf` uid_s
        ]

      -- If the haskell-debugger-view is in the dependency graph, it must have
      -- one of the versions the debugger is known to support:
      supported_ranges -- [min, max(
        = [ (makeVersion [0, 2], makeVersion [0, 3]) ]

  case hskl_dbgr_vws of
    [hdv_uid] -> do
      -- In transitive closure, use that one.
      -- Check that the version is in supported range.
      case lookupUnit unitState (RealUnit (Definite hdv_uid)) of
        Just unitInfo -> do
          let version = unitPackageVersion unitInfo
          if any (\(l,h) -> l <= version && version < h) supported_ranges
            then return (Just hdv_uid)
            else throwM UnsupportedHsDbgViewVersion{supportedVersions=supported_ranges, actualVersion=version}
        Nothing
          | "inplace" `L.isSuffixOf` unitIdString hdv_uid
          -- will be built as a target later
          -> return (Just hdv_uid)
        Nothing ->
          error "Could not find unit info for haskell-debugger-view"
    [] -> do
      return Nothing
    _  -> do
      error $ "Multiple unit-ids found for haskell-debugger-view in the transitive closure?!" ++ showSDocUnsafe (withPprStyle (PprDump alwaysQualify) (ppr hskl_dbgr_vws))

--------------------------------------------------------------------------------
-- * Modules
--------------------------------------------------------------------------------

-- | List all loaded modules 'ModSummary's
getAllLoadedModules :: GHC.GhcMonad m => m [GHC.ModuleNodeInfo]
getAllLoadedModules =
  (mgInfos . mg_mss <$> GHC.getModuleGraph) >>=
    filterM (\ms -> GHC.isLoadedModule (moduleNodeInfoUnitId ms) (moduleNodeInfoModuleName ms))
  where
    mgInfos xs = [ info | ModuleNode _ info <- xs ]

getAllLoadedModulesWithPaths :: GHC.GhcMonad m =>
  m [(AbsFilePath,GHC.ModuleNodeInfo)]
getAllLoadedModulesWithPaths = do
  ghcCwd <- mkAbsolute <$> liftIO getCurrentDirectory
  -- TODO: cache?
  map (\ m -> (absoluteSourcePath ghcCwd m, m)) <$> getAllLoadedModules
  where
    absoluteSourcePath :: AbsFilePath -> ModuleNodeInfo -> AbsFilePath
    absoluteSourcePath ghcCwdDir ms
      = ghcCwdDir /> (fromMaybe (error $ "missing source path: " ++ show (moduleNodeInfoModuleName ms)) $ ml_hs_file (moduleNodeInfoLocation ms))
