{-# LANGUAGE DerivingStrategies, CPP, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Initialise the GHC session for one or more home units.
--
-- This code is inspired of HLS's session initialisation.
-- It would be great to extract common functions in the future.
module GHC.Debugger.Session (
  parseHomeUnitArguments,
  setupHomeUnitGraph,
  validateUnitsWays,
  TargetDetails(..),
  Target(..),
  toGhcTarget,
  CacheDirs(..),
  getCacheDirs,
  -- * Debugger's Interactive Home Unit
  interactiveGhcDebuggerUnitId,
  getInteractiveDebuggerDynFlags,
  setInteractiveDebuggerDynFlags,
  -- * DynFlags modifications
  setWorkingDirectory,
  setCacheDirs,
  setBytecodeBackend,
  enableByteCodeGeneration,
  enableExternalInterpreter,
  enableDynamicDebuggee,
  setPgmI, addOptI,
  setDynFlagWays,
  makeDynFlagsAbsoluteOverall,
  resumeExec,
  setExposedInUnit,
  graphUnits,
  )
  where

#if MIN_VERSION_ghc(9,14,2)
import Data.Function ((&))
#endif
import Control.Monad
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                    as H
import qualified Data.ByteString.Base16              as B16
import qualified Data.ByteString.Char8               as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Containers.ListUtils as L
import GHC.ResponseFile (expandResponse)
import HIE.Bios.Environment as HIE
import System.FilePath
import Data.Time
import qualified System.Directory as Directory
import qualified System.Environment as Env

import qualified GHC
import GHC.Platform.Ways
import GHC.Driver.DynFlags as GHC
import GHC.Driver.Monad
import qualified GHC.Driver.Session as GHC
import GHC.Utils.Monad as GHC
import GHC.Unit.Home.Graph
import GHC.Unit.Home.PackageTable
import GHC.Unit.Env
import GHC.Unit.Types
import qualified GHC.Unit.State                        as State
import GHC.Driver.Env
import GHC.Types.SrcLoc
import GHC.Settings (ToolSettings(..))
import Language.Haskell.Syntax.Module.Name
import qualified Data.Foldable as Foldable
import qualified GHC.Unit.Home.Graph as HUG
import qualified Data.Set as Set
import Data.Maybe
import GHC.Types.Target (InputFileBuffer)
import GHC (SingleStep, ExecResult)
import Data.Set (Set)
import qualified GHC.Unit as GHC
import GHC.Unit.Module.Graph (mg_mss, ModuleGraphNode (..), ModNodeKeyWithUid (mnkUnitId), mnKey)

-- | Throws if package flags are unsatisfiable
parseHomeUnitArguments :: GhcMonad m
    => FilePath -- ^ Main entry point function
    -> FilePath -- ^ Component root. Important for multi-package cabal projects.
    -> [String]
    -> [String] -- ghcInvocation
    -> DynFlags
    -> FilePath -- ^ root dir, see Note [Root Directory]
    -> m (NonEmpty.NonEmpty (DynFlags, [GHC.Target]))
parseHomeUnitArguments cfp compRoot units theOpts dflags rootDir = do
    ((theOpts',_errs,_warns),_units) <- GHC.processCmdLineP [] [] (map noLoc theOpts)
    case NonEmpty.nonEmpty units of
      Just us -> initMulti us
      Nothing -> do
        (df, targets) <- initOne (map unLoc theOpts')
        -- A special target for the file which caused this wonderful
        -- component to be created. In case the cradle doesn't list all the targets for
        -- the component, in which case things will be horribly broken anyway.
        --
        -- When we have a singleComponent that is caused to be loaded due to a
        -- file, we assume the file is part of that component. This is useful
        -- for bare GHC sessions, such as many of the ones used in the testsuite
        --
        -- We don't do this when we have multiple components, because each
        -- component better list all targets or there will be anarchy.
        -- It is difficult to know which component to add our file to in
        -- that case.
        -- Multi unit arguments are likely to come from cabal, which
        -- does list all targets.
        --
        -- If we don't end up with a target for the current file in the end, then
        -- we will report it as an error for that file
        let abs_fp = rootDir </> cfp
        -- Canonicalize! Why? Because the targets we get from the cradle are normalised and if we don't normalise the "special target" then they aren't deduplicated properly.
        canon_fp <- liftIO $ Directory.canonicalizePath abs_fp
        let special_target = mkSimpleTarget df canon_fp
        pure $ (df, if null targets then [special_target] else targets) NonEmpty.:| []
    where
      initMulti unitArgFiles =
        forM unitArgFiles $ \f -> do
          args <- liftIO $ expandResponse [f]
          initOne args
      initOne this_opts = do
        (dflags', targets') <- addCmdOpts this_opts dflags
        let root = case workingDirectory dflags' of
              Nothing   -> compRoot
              Just wdir -> compRoot </> wdir
        root_canon <- liftIO $ Directory.canonicalizePath root
        let targets = HIE.makeTargetsAbsolute root_canon targets'
        cacheDirs <- liftIO $ getCacheDirs (takeFileName root) this_opts
        let dflags'' =
              makeDynFlagsDirsAbsolute compRoot $
              setWorkingDirectory root $
              setCacheDirs cacheDirs $
              enableByteCodeGeneration $
              setBytecodeBackend $
              makeDynFlagsAbsolute compRoot -- makeDynFlagsAbsolute already accounts for workingDirectory
              dflags'
        return (dflags'', targets)

setupHomeUnitGraph :: GhcMonad m => [(DynFlags, [GHC.Target])] -> m ()
setupHomeUnitGraph flagsAndTargets = do
  hsc_env <- GHC.getSession
  (hsc_env', targetDetails) <- liftIO $ setupMultiHomeUnitGhcSession [".hs", ".lhs"] hsc_env flagsAndTargets
  GHC.setSession hsc_env'
  GHC.setTargets (fmap toGhcTarget targetDetails)

-- | Set up the 'HomeUnitGraph' with empty 'HomeUnitEnv's.
-- The first 'DynFlags' are the 'DynFlags' for the interactive session.
createHomeUnitGraph :: GHC.Logger -> [DynFlags] -> IO HomeUnitGraph
createHomeUnitGraph logger unitDflags = do
  let home_units = Set.fromList $ map homeUnitId_ unitDflags
  unitEnvList <- flip traverse unitDflags $ \ dflags -> do
    hue <- setupNewHomeUnitEnv logger dflags Nothing home_units
    pure (homeUnitId_ dflags, hue)

  pure $ unitEnv_new (Map.fromList unitEnvList)

setupNewHomeUnitEnv :: GHC.Logger -> DynFlags -> Maybe [GHC.UnitDatabase UnitId] -> Set UnitId -> IO HomeUnitEnv
setupNewHomeUnitEnv logger dflags cached_dbs other_home_units = do
  emptyHpt <- emptyHomePackageTable
  (dbs,unit_state,home_unit,mconstants) <- State.initUnits logger dflags cached_dbs other_home_units
  updated_dflags <- GHC.updatePlatformConstants dflags mconstants
  pure $ mkHomeUnitEnv unit_state (Just dbs) updated_dflags emptyHpt (Just home_unit)

-- | Given a set of 'DynFlags', set up the 'UnitEnv' and 'HomeUnitEnv' for this
-- 'HscEnv'.
-- We assume the 'HscEnv' is "empty", e.g. wasn't already used to compile
-- anything.
initHomeUnitEnv :: [DynFlags] -> HscEnv -> IO HscEnv
initHomeUnitEnv unitDflags env = do
  let dflags0         = hsc_dflags env

  initial_home_graph <- createHomeUnitGraph (hsc_logger env) unitDflags

  -- We set up the interactive debugger home unit after the other home units
  -- have been initialised.
  -- This allows us to reuse the package databases and their respective visibilities.
  interactiveHomeUnit <- do
    let
      home_units = unitEnv_keys initial_home_graph

      interactiveDynFlags = dflags0
        { homeUnitId_ = interactiveGhcDebuggerUnitId
        , importPaths = []
        , packageFlags =
            [ ExposePackage
                (unitIdString home_unit_id)
                (UnitIdArg $ RealUnit (Definite home_unit_id))
                (ModRenaming True [])
            | home_unit_id <- Set.toList home_units
            ]
        }

    let cached_unit_dbs = concat . catMaybes . fmap homeUnitEnv_unit_dbs $ Foldable.toList initial_home_graph
    setupNewHomeUnitEnv (hsc_logger env) interactiveDynFlags (Just cached_unit_dbs) home_units

  let home_unit_graph =
        HUG.unitEnv_insert interactiveGhcDebuggerUnitId interactiveHomeUnit initial_home_graph

  let interactiveDFlags = homeUnitEnv_dflags interactiveHomeUnit
  unit_env <-
    initUnitEnv interactiveGhcDebuggerUnitId home_unit_graph (GHC.ghcNameVersion interactiveDFlags) (targetPlatform interactiveDFlags)
  pure $ hscSetFlags interactiveDFlags $ hscSetUnitEnv unit_env env

-- | Extracts @UnitId@s from the graph.
graphUnits :: GHC.ModuleGraph -> [UnitId]
graphUnits mod_graph = L.nubOrd .
  (`mapMaybe` mg_mss mod_graph) $ \case
         UnitNode _deps uid -> Just uid
         ModuleNode _ modl -> Just $ mnkUnitId $ mnKey modl
         InstantiationNode uid _ -> Just uid
         LinkNode _ _ -> Nothing

-- | Rebuilds the UnitState of the unit, exposing the given packages.
--
--   Takes care of updating hsc_dflags, ue_platform, and ue_namever if this is the ue_currentUnit.
setExposedInUnit :: UnitId -> [UnitId] -> Ghc ()
setExposedInUnit unitId exposed = do
  env <- GHC.getSession
  let old_ie = case lookupHugUnitId unitId (hsc_HUG env) of
        Just hue -> hue
        Nothing -> error $ "setExposedInUnit: unit not found " ++ unitIdString unitId

  let dflags = (homeUnitEnv_dflags old_ie) { packageFlags = [ExposePackage
                  (unitIdString uid)
                  (UnitIdArg $ RealUnit (Definite uid))
                  (ModRenaming True [])
              | uid <- exposed
              , uid /= rtsUnitId
              , uid /= ghcInternalUnitId
              , unitIdString uid /= "haskell-debugger-view-in-memory"
              -- FIXME: any other to filter out?
              ]}
  let cached_dbs = homeUnitEnv_unit_dbs old_ie
  let home_units = Set.fromList $ State.homeUnitDepends $ homeUnitEnv_units old_ie
  (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits (hsc_logger env) dflags cached_dbs home_units

  updated_dflags <- liftIO $ GHC.updatePlatformConstants dflags mconstants
  let ie = old_ie
       { homeUnitEnv_units = unit_state
       , homeUnitEnv_unit_dbs = Just dbs
       , homeUnitEnv_dflags = updated_dflags
       , homeUnitEnv_home_unit = Just home_unit
       }

  let home_unit_graph = HUG.unitEnv_insert unitId ie (hsc_HUG env)
  let ue0 = hsc_unit_env env
  let ue1 = ue0 {ue_home_unit_graph = home_unit_graph}
  let
    new_env
      | ue_currentUnit ue1 /= unitId = hscSetUnitEnv ue1 env
      | otherwise = hscSetFlags dflags1 $ hscSetUnitEnv ue2 env
      where
        dflags1 = homeUnitEnv_dflags $ unitEnv_lookup unitId (ue_home_unit_graph ue1)
        ue2 = ue1
          { ue_platform        = targetPlatform dflags1
          , ue_namever         = GHC.ghcNameVersion dflags1
          }
  GHC.setSession new_env

-- | Setup the given 'HscEnv' to hold a 'UnitEnv'
-- with all the given components.
-- We return the modified 'HscEnv' and all the 'TargetDetails' for
-- the given 'GHC.Target's.
setupMultiHomeUnitGhcSession
         :: [String]           -- ^ File extensions to consider. This is mostly a remnant of HLS.
         -> HscEnv             -- ^ An empty HscEnv that we can use the setup the session.
         -> [(DynFlags, [GHC.Target])]    -- ^ New components to be loaded. Expected to be non-empty.
         -> IO (HscEnv, [TargetDetails])
setupMultiHomeUnitGhcSession exts hsc_env cis = do
    let dfs = map fst cis

    hscEnv' <- initHomeUnitEnv dfs hsc_env
    -- TODO: this should be reported
    -- _ <- maybeToList $ GHC.checkHomeUnitsClosed (hsc_unit_env hscEnv') (hsc_all_home_unit_ids hscEnv')
    ts <- forM cis $ \(df, targets) -> do
      -- evaluate $ liftRnf rwhnf targets

      let mk t = fromTargetId (importPaths df) exts (homeUnitId_ df) (GHC.targetId t) (GHC.targetContents t)
      ctargets <- concatMapM mk targets

      return (L.nubOrdOn targetTarget ctargets)
    pure (hscEnv', concat ts)

-- | Find and return the ways in which the home units are built.
-- INVARIANT: All home units are built with the same 'Ways'
validateUnitsWays :: NonEmpty.NonEmpty DynFlags -> IO Ways
validateUnitsWays flags = do
    let unitWays  = NonEmpty.map ways flags
        firstWays = NonEmpty.head unitWays
        restWays  = NonEmpty.tail unitWays
    if all (== firstWays) restWays
        then return firstWays
        else error "Unexpected: the home units have different ways! Not supported, see GHC#26765"

data TargetDetails = TargetDetails
  { targetTarget :: Target
  -- ^ Simplified version of 'TargetId', storing enough information
  --
  , targetLocations :: [FilePath]
  -- ^ The physical location of 'targetTarget'.
  -- Contains '-boot' file locations.
  -- At this moment in time, these are unused, but could be used to create
  -- convenient lookup table from 'FilePath' to 'TargetDetails'.
  , targetUnitId :: UnitId
  -- ^ UnitId of 'targetTarget'.
  , targetContents :: Maybe (InputFileBuffer, UTCTime)
  }

-- | A simplified view on a 'TargetId'.
--
-- Implements 'Ord' and 'Show' which can be convenient.
data Target = TargetModule ModuleName | TargetFile FilePath
  deriving ( Eq, Ord, Show )

-- | Turn a 'TargetDetails' into a 'GHC.Target'.
toGhcTarget :: TargetDetails -> GHC.Target
toGhcTarget (TargetDetails tid _ uid cts) = case tid of
  TargetModule modl -> GHC.Target (GHC.TargetModule modl) True uid cts
  TargetFile fp -> GHC.Target (GHC.TargetFile fp Nothing) True uid cts

fromTargetId :: [FilePath]          -- ^ import paths
             -> [String]            -- ^ extensions to consider
             -> UnitId
             -> GHC.TargetId
             -> Maybe (InputFileBuffer, UTCTime)
             -> IO [TargetDetails]
-- For a target module we consider all the import paths
fromTargetId is exts unitId (GHC.TargetModule modName) ctts = do
    let fps = [i </> moduleNameSlashes modName -<.> ext <> boot
              | ext <- exts
              , i <- is
              , boot <- ["", "-boot"]
              ]
    return [TargetDetails (TargetModule modName) fps unitId ctts]
-- For a 'TargetFile' we consider all the possible module names
fromTargetId _ _ unitId (GHC.TargetFile f _) ctts = do
    let other
          | "-boot" `L.isSuffixOf` f = dropEnd 5 f
          | otherwise = (f ++ "-boot")
    return [TargetDetails (TargetFile f) [f, other] unitId ctts]

-- ----------------------------------------------------------------------------
-- GHC Utils that should likely be exposed by GHC
-- ----------------------------------------------------------------------------

mkSimpleTarget :: DynFlags -> FilePath -> GHC.Target
mkSimpleTarget df fp = GHC.Target (GHC.TargetFile fp Nothing) True (homeUnitId_ df) Nothing

hscSetUnitEnv :: UnitEnv -> HscEnv -> HscEnv
hscSetUnitEnv ue env = env { hsc_unit_env = ue }

-- ----------------------------------------------------------------------------
-- Session cache directory
-- ----------------------------------------------------------------------------

data CacheDirs = CacheDirs
  { hiCacheDir :: FilePath
  , byteCodeCacheDir :: FilePath
  , hieCacheDir :: FilePath
  , objCacheDir :: FilePath
  }

getCacheDirs :: String -> [String] -> IO CacheDirs
getCacheDirs prefix opts = do
  mCacheDir <- Env.lookupEnv "HDB_CACHE_DIR"
  rootDir <- case mCacheDir of
    Just dir -> pure dir
    Nothing ->
      Directory.getXdgDirectory Directory.XdgCache "hdb"
  let sessionCacheDir = rootDir </> prefix ++ "-" ++ opts_hash
  Directory.createDirectoryIfMissing True sessionCacheDir
  pure CacheDirs
    { hiCacheDir = sessionCacheDir
    , byteCodeCacheDir = sessionCacheDir
    , hieCacheDir = sessionCacheDir
    , objCacheDir = sessionCacheDir
    }
  where
    -- Create a unique folder per set of different GHC options, assuming that each different set of
    -- GHC options will create incompatible interface files.
    opts_hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- ----------------------------------------------------------------------------
-- The Interactive DynFlags
-- ----------------------------------------------------------------------------

interactiveGhcDebuggerUnit :: Unit
interactiveGhcDebuggerUnit = stringToUnit "interactiveGhcDebugger"

interactiveGhcDebuggerUnitId :: UnitId
interactiveGhcDebuggerUnitId = toUnitId interactiveGhcDebuggerUnit

getInteractiveDebuggerDynFlags :: GhcMonad m => m DynFlags
getInteractiveDebuggerDynFlags = do
  env <- getSession
  pure $ ue_unitFlags interactiveGhcDebuggerUnitId (hsc_unit_env env)

-- | Set the interactive 'DynFlags' for the haskell-debugger session.
-- We manage a separate home unit for the interactive 'DynFlags'.
-- The invariant is that 'DynFlags' found in 'InteractiveContext' *must* be
-- the same 'DynFlags' as the ones found in 'interactiveGhcDebuggerUnitId' in
-- the 'HomeUnitEnv'
-- This function upholds this invariant.
--
-- Always prefer this, over 'setInteractiveDynFlags'.
setInteractiveDebuggerDynFlags :: GhcMonad m => DynFlags -> m ()
setInteractiveDebuggerDynFlags dflags = do
  env <- getSession
  norm_dflags <- GHC.normaliseInteractiveDynFlags (hsc_logger env) dflags
  env' <- GHC.initialiseInteractiveDynFlags norm_dflags env
  -- Make sure the 'InteractiveContext' and 'interactiveGhcDebuggerUnitId' have exactly
  -- the same 'DynFlags'
  let newEnv =
        if homeUnitId_ (hsc_dflags env') == interactiveGhcDebuggerUnitId
          then hscSetFlags norm_dflags env'
          else
            let
              unit_env = hsc_unit_env env'
            in env'
                { hsc_unit_env = unit_env
                    { ue_home_unit_graph =
                        updateUnitFlags
                          interactiveGhcDebuggerUnitId
                          (const norm_dflags)
                          (ue_home_unit_graph unit_env)
                    }
                }
  setSession newEnv

-- ----------------------------------------------------------------------------
-- Modification of DynFlags
-- ----------------------------------------------------------------------------

setWorkingDirectory :: FilePath -> DynFlags -> DynFlags
setWorkingDirectory p d = d { workingDirectory =  Just p }

setCacheDirs :: CacheDirs -> DynFlags -> DynFlags
setCacheDirs CacheDirs{..} flags = flags
  { hiDir = Just hiCacheDir
  , hieDir = Just hieCacheDir
  , objectDir = Just objCacheDir
#if MIN_VERSION_ghc(9,14,2)
  , bytecodeDir = Just byteCodeCacheDir
#endif
  }

-- | Prefixes output directories (i.e. @hiDir@, @hieDir@, @stubDir@, @dumpDir@) with @rootDir@ argument.
makeDynFlagsDirsAbsolute :: FilePath -> DynFlags -> DynFlags
makeDynFlagsDirsAbsolute rootDir df0 =
  foldl' (\ df f -> f (fmap $ normalise . (rootDir </>)) df) df0
    [(\ f df -> df {hiDir = f (hiDir df)})
    ,(\ f df -> df {hieDir = f (hieDir df)})
    ,(\ f df -> df {stubDir = f (stubDir df)})
    ,(\ f df -> df {dumpDir = f (dumpDir df)})]

makeDynFlagsAbsoluteOverall :: FilePath -> DynFlags -> DynFlags
makeDynFlagsAbsoluteOverall rootDir df0 = makeDynFlagsDirsAbsolute rootDir $ makeDynFlagsAbsolute rootDir df0

-- | If the compiler supports `.gbc` files (>= 9.14.2), then persist these
-- artefacts to disk.
enableByteCodeGeneration :: DynFlags -> DynFlags
enableByteCodeGeneration dflags =
#if MIN_VERSION_ghc(9,14,2)
  dflags
    & flip gopt_unset Opt_ByteCodeAndObjectCode
    & flip gopt_set Opt_ByteCode
    & flip gopt_set Opt_WriteByteCode
    & flip gopt_set Opt_WriteInterface
#else
  dflags
#endif

setBytecodeBackend :: DynFlags -> DynFlags
setBytecodeBackend dflags = dflags
  {
#if MIN_VERSION_ghc(9,14,2)
  backend = GHC.bytecodeBackend
#else
  backend = GHC.interpreterBackend
#endif
  }

-- | Enable the external interpreter by default unless the user sets
-- @preferInternalInterpreter=True@ (with @--internal-interpreter@)
enableExternalInterpreter :: Bool -> DynFlags -> DynFlags
enableExternalInterpreter preferInternalInterpreter dflags
  | preferInternalInterpreter
  = dflags `GHC.gopt_unset` GHC.Opt_ExternalInterpreter
  | otherwise
  = dflags `GHC.gopt_set` GHC.Opt_ExternalInterpreter

-- | Force -dynamic on the debuggee if the debugger (which is also the external
-- interpreter) was compiled with -dynamic. On Windows the debugger can't be
-- built dynamic, so we won't enable it there.
--
-- See Note [Dynamic Debuggee for dynamic debugger]
enableDynamicDebuggee :: DynFlags -> DynFlags
enableDynamicDebuggee dflags
  | hostIsDynamic
  = addWay' WayDyn dflags
  | otherwise
  = dflags

setPgmI, addOptI :: String -> DynFlags -> DynFlags
setPgmI f = alterToolSettings $ \s -> s { toolSettings_pgm_i = f }
addOptI f = alterToolSettings $ \s -> s { toolSettings_opt_i = f : toolSettings_opt_i s }

alterToolSettings :: (ToolSettings -> ToolSettings) -> DynFlags -> DynFlags
alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }

setDynFlagWays :: Ways -> DynFlags -> DynFlags
setDynFlagWays ws dyn = Set.foldr addWay' dyn ws

addWay' :: Way -> DynFlags -> DynFlags
addWay' w dflags0 =
   let platform = targetPlatform dflags0
       dflags1 = dflags0 { targetWays_ = addWay w (targetWays_ dflags0) }
       dflags2 = foldr GHC.setGeneralFlag' dflags1
                       (wayGeneralFlags platform w)
       dflags3 = foldr GHC.unSetGeneralFlag' dflags2
                       (wayUnsetGeneralFlags platform w)
   in dflags3

-- ----------------------------------------------------------------------------
-- Wrappers around GHC's odd behavior
-- ----------------------------------------------------------------------------

resumeExec :: GhcMonad m => SingleStep -> Maybe Int -> m ExecResult
resumeExec a b = do
  -- IC's ic_imports field is not kept in sync with ic_gre_cache, so we could do
  -- this call later, but why rely on that.
  imports <- GHC.getContext

  v <- GHC.resumeExec a b

  -- To have interactive imports persist after a `continue` command we have to
  -- work around how GHC.resumeExec handles the InteractiveContext (IC).
  --
  -- GHC.resumeExec resets the scope of the IC (i.e. ic_gre_cache) to what it
  -- was before the last ExecBreak.
  --
  -- It makes sense for GHC.resumeExec to remove from the IC scope the
  -- breakpoint locals and anything that could have been defined with them, in
  -- fact they are also unloaded.
  --
  -- The way it's done though also rollbacks any import statements that were
  -- executed since the last ExecBreak. The only fix is to reimport everything
  -- again.
  --
  -- Note: GHC.setContext recomputes the scope of the interactive imports from
  -- scratch everytime. Considering `runDebugger` adds the whole home unit to
  -- the interactive imports this might become a bottleneck. GHC does not keep
  -- any reference to the scope containing just the imports, so we would have to
  -- cache it ourselves (and then extend it with the cached scope of
  -- ic_tythings, i.e. igre_prompt_env (c.f. replaceImportEnv)).
  GHC.setContext imports
  pure v

-- ----------------------------------------------------------------------------
-- Utils that we need, but don't want to incur an additional dependency for.
-- ----------------------------------------------------------------------------

-- | Drop a number of elements from the end of the list.
--
-- > dropEnd 3 "hello"  == "he"
-- > dropEnd 5 "bye"    == ""
-- > dropEnd (-1) "bye" == "bye"
-- > \i xs -> dropEnd i xs `isPrefixOf` xs
-- > \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
-- > \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
dropEnd :: Int -> [a] -> [a]
dropEnd i xs
    | i <= 0 = xs
    | otherwise = f xs (drop i xs)
    where f (a:as) (_:bs) = a : f as bs
          f _ _ = []
