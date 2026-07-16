{-# LANGUAGE DerivingStrategies, CPP, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}

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
  exposeModGraphUnitsInInteractiveGhcDebuggerUnit,
  graphUnits,
  compileModuleWithDepsInHpt,
  home_unit_dflags,
  packageImportDecl,
  withUnliftGhc,
  annotateCallStackGhc,
  lookupUnitPackageQualifier,
  fixHomeUnitsDynFlagsForIIDecl, getPgmI,
  initUniqSupplyIO
  )
  where

#if MIN_VERSION_ghc(9,14,2)
import Data.Function ((&))
#endif
import Control.Applicative ((<|>))
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                    as H
import qualified Data.ByteString.Base16              as B16
import qualified Data.ByteString.Char8               as B
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Containers.ListUtils as ListUtils
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
import Language.Haskell.Syntax.Module.Name
import qualified Data.Foldable as Foldable
import qualified GHC.Unit.Home.Graph as HUG
import qualified Data.Set as Set
import Data.Maybe
import GHC.Types.Target (InputFileBuffer)
import GHC (SingleStep, ExecResult, ModSummary (ms_hspp_opts), ideclPkgQual, ImportDecl, GhcPs)
import Data.Set (Set)
import qualified GHC.Unit as GHC
import GHC.Unit.Module.Graph (mg_mss, ModuleGraphNode (..), mnKey)
import GHC.Driver.Make
import GHC.Unit.Home.ModInfo (HomeModInfo(..))
import qualified GHC.Driver.Errors.Types as GHC
import System.Directory (doesFileExist)
import qualified GHC.Types.Error as GHC
import qualified GHC.Utils.Error as GHC
import GHC.Driver.Pipeline (compileOne)
import qualified GHC.Unit.Home.ModInfo as GHC
import GHC.Utils.TmpFs
import Data.Foldable (for_)
import GHC.Plugins (SourceError, try, RawPkgQual (..), HasCallStack, FastString, mkFastString, lookupUnitId)
import GHC.Types.SourceText (StringLiteral(..), SourceText (..))
import GHC.Stack.Annotation
import GHC.Stack (callStack)
import GHC.Settings (ToolSettings(..))
import qualified GHC.Types.Unique.Supply as GHC

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
    let uid = homeUnitId_ dflags
    hue <- setupNewHomeUnitEnv home_units logger dflags Nothing
    assert (homeUnitId_ (homeUnitEnv_dflags hue) == uid) $
      pure (uid, hue)

  pure $ unitEnv_new (Map.fromList unitEnvList)

-- | See Note [ Ambiguous Package Qualified Imports Workaround ]
fixHomeUnitsDynFlagsForIIDecl :: Ghc ()
fixHomeUnitsDynFlagsForIIDecl = do
  modifySession $ hscUpdateHUG $ \ hug -> do
    let manyHomeUnits = Set.size (HUG.allUnits hug) > 1
    let h hue = hue { homeUnitEnv_dflags = fixFlagsForIIDecl manyHomeUnits (homeUnitEnv_dflags hue) }
    runIdentity . unitEnv_traverseWithKey (const $ pure . h) $ hug
  where
    -- | Makes package names of home units unique and removes hidden modules.
    fixFlagsForIIDecl :: Bool -> DynFlags -> DynFlags
    fixFlagsForIIDecl False df | Just{} <- thisPackageName df = df {hiddenModules = mempty}
    -- TODO #288: pick more user-friendly names.
    fixFlagsForIIDecl _manyHUnits dflags = dflags { thisPackageName = Just (unitIdString (homeUnitId_ dflags))
        , hiddenModules = mempty}

-- | The first argument should contain the home units the new @HomeUnitEnv@ depends on (@allUnits (hsc_HUG env)@ is always safe to give).
--   The actual dependencies are specified by the @packageFlags@ in the @DynFlags@ argument.
setupNewHomeUnitEnv :: Set UnitId -> GHC.Logger -> DynFlags -> Maybe [GHC.UnitDatabase UnitId] -> IO HomeUnitEnv
setupNewHomeUnitEnv hug_keys logger dflags cached_dbs = do
  emptyHpt <- emptyHomePackageTable
  (dbs,unit_state,home_unit,mconstants) <- State.initUnits logger dflags cached_dbs hug_keys
  updated_dflags <- GHC.updatePlatformConstants dflags mconstants
  pure $ mkHomeUnitEnv unit_state (Just dbs) updated_dflags emptyHpt (Just home_unit)

-- | Given a set of 'DynFlags', set up the 'UnitEnv' and 'HomeUnitEnv' for this
-- 'HscEnv'.
-- We assume the 'HscEnv' is "empty", e.g. wasn't already used to compile
-- anything.
initHomeUnitEnv :: [DynFlags] -> HscEnv -> IO HscEnv
initHomeUnitEnv unitDflags env = do

  initial_home_graph <- createHomeUnitGraph (hsc_logger env) unitDflags

  -- We need one of the units to be the `ue_currentUnit`: by default it's "main", but we don't create such a unit and Ghc panics.
  addInteractiveGhcDebuggerUnit (Set.toList . allUnits $ initial_home_graph) $ hscUpdateHUG (const initial_home_graph) env

-- | Adds or refreshes the @interactiveGhcDebuggerUnit@ passing the first
-- argument as @ExposePackage@ flags.
addInteractiveGhcDebuggerUnit :: [UnitId] -> HscEnv -> IO HscEnv
addInteractiveGhcDebuggerUnit exposed env = do
  let dflags0 = hsc_dflags env
  let initial_home_graph = hsc_HUG env
  -- We set up the interactive debugger home unit after the other home units
  -- have been initialised.
  -- This allows us to reuse the package databases and their respective visibilities.
  interactiveHomeUnit <- do
    let
      interactiveDynFlags = dflags0
        { homeUnitId_ = interactiveGhcDebuggerUnitId
        , importPaths = []
        , packageFlags =
            [ ExposePackage
                (unitIdString uid)
                (UnitIdArg $ RealUnit (Definite uid))
                (ModRenaming True [])
            | uid <- exposed
            , uid /= rtsUnitId
            , uid /= ghcInternalUnitId
            , uid /= interactiveGhcDebuggerUnitId
            -- TODO: other uids to filter?
            ]
        }

    let cached_unit_dbs = concat . catMaybes . fmap homeUnitEnv_unit_dbs $ Foldable.toList initial_home_graph
    setupNewHomeUnitEnv (allUnits initial_home_graph) (hsc_logger env) interactiveDynFlags (Just cached_unit_dbs)

  let home_unit_graph =
        HUG.unitEnv_insert interactiveGhcDebuggerUnitId interactiveHomeUnit initial_home_graph

  let interactiveDFlags = homeUnitEnv_dflags interactiveHomeUnit
  let unit_env = (hsc_unit_env env)
        { ue_home_unit_graph = home_unit_graph
        , ue_current_unit    = interactiveGhcDebuggerUnitId
        , ue_platform        = targetPlatform interactiveDFlags
        , ue_namever         = GHC.ghcNameVersion interactiveDFlags
        }
  pure $ hscSetFlags interactiveDFlags $ hscSetUnitEnv unit_env env

-- | Sets the units from the @ModuleGraph@ as the exposed ones for @InteractiveGhcDebuggerUnit@.
--
--   See Note [Must explicitly expose module graph units].
exposeModGraphUnitsInInteractiveGhcDebuggerUnit :: Ghc ()
exposeModGraphUnitsInInteractiveGhcDebuggerUnit =
  modifySessionM $ \ env -> do
    liftIO $ addInteractiveGhcDebuggerUnit (graphUnits . hsc_mod_graph $ env) env

-- | Extracts @UnitId@s from the graph.
graphUnits :: GHC.ModuleGraph -> [UnitId]
graphUnits mod_graph = ListUtils.nubOrd .
  (`mapMaybe` mg_mss mod_graph) $ \case
         UnitNode _deps uid -> Just uid
         ModuleNode _ modl -> Just $ mnkUnitId $ mnKey modl
         InstantiationNode uid _ -> Just uid
         LinkNode _ _ -> Nothing

-- | WARNING: callback is not to be used from other threads.
withUnliftGhc :: ((Ghc b -> IO b) -> IO a) -> Ghc a
withUnliftGhc k = reifyGhc $ \ s -> k (flip reflectGhc s)

annotateCallStackGhc :: HasCallStack => Ghc a -> Ghc a
annotateCallStackGhc m = let x = callStack in withUnliftGhc $ \k -> annotateStackShowIO x $ k m


-- | Setup the given 'HscEnv' to hold a 'UnitEnv'
-- with all the given components.
-- We return the modified 'HscEnv' and all the 'TargetDetails' for
-- the given 'GHC.Target's.
setupMultiHomeUnitGhcSession
         :: [String]           -- ^ File extensions to consider. This is mostly a remnant of HLS.
         -> HscEnv             -- ^ An empty HscEnv that we can use the setup the session.
         -> [(DynFlags, [GHC.Target])]    -- ^ New components to be loaded. Expected to be non-empty.
         -> IO (HscEnv, [TargetDetails])
setupMultiHomeUnitGhcSession exts hsc_env cis = annotateCallStackIO $ do
    let dfs = map fst cis

    hscEnv' <- initHomeUnitEnv dfs hsc_env
    -- TODO: this should be reported
    -- _ <- maybeToList $ GHC.checkHomeUnitsClosed (hsc_unit_env hscEnv') (hsc_all_home_unit_ids hscEnv')
    ts <- forM cis $ \(df, targets) -> do
      -- evaluate $ liftRnf rwhnf targets

      let mk t = fromTargetId (importPaths df) exts (homeUnitId_ df) (GHC.targetId t) (GHC.targetContents t)
      ctargets <- concatMapM mk targets

      return (ListUtils.nubOrdOn targetTarget ctargets)
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

{-
Note [ Ambiguous Package Qualified Imports Workaround ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Source level package qualified imports `import "foo" A` interpret "foo" as a package name.

When one manually builds a `RawPkgQual` for an `ImportDecl` one can get away with using a unit-id, but only for external (i.e. not home) units.
That it works does not seem entirely intended (see quoted snippet below), the code is in `renamePkgQual`: If a package qualifier is not found among packages it's looked up as an external unit. This is already in the code path for `OtherPkg` though, which is why Home Units are excluded.
```
    | otherwise
    -> OtherPkg (UnitId pkg_fs)
       -- not really correct as pkg_fs is unlikely to be a valid unit-id but
       -- we will report the failure later...
```

Home units will only be found if the qualifier matches their dflags' `thisPackageName`. However that's bugged because the lookup doesn't bother considering there can be multiple units in the same package (library, sublibraries and exe units), and just picks the first found, leading to an import error if e.g. the library unit is picked but the module was in the exe one.
Related GHC issue: https://gitlab.haskell.org/ghc/ghc/-/issues/24227

Turns out that the package name of a home unit is pretty meaningless though, so we can update the dflags to replace it with anything that's actually unique so we can dodge the bug.

Another stumbling block is that the `IIDecl` mode of an `InteractiveImport` does not allow importing hidden modules, but again for home units we can alter the DynFlags so all modules are exposed.

See issue #288 for what can we do for users at the repl.
-}

-- ----------------------------------------------------------------------------
-- GHC Utils that should likely be exposed by GHC
-- ----------------------------------------------------------------------------

mkSimpleTarget :: DynFlags -> FilePath -> GHC.Target
mkSimpleTarget df fp = GHC.Target (GHC.TargetFile fp Nothing) True (homeUnitId_ df) Nothing

hscSetUnitEnv :: UnitEnv -> HscEnv -> HscEnv
hscSetUnitEnv ue env = env { hsc_unit_env = ue }

home_unit_dflags :: HscEnv -> UnitId -> Maybe DynFlags
home_unit_dflags hsc_env uid
  = fmap homeUnitEnv_dflags
  . HUG.lookupHugUnitId uid . ue_home_unit_graph
  . hsc_unit_env
  $ hsc_env

-- | See Note [Package Qualified Imports] for why this is sometimes a @PackageName@ and sometimes a @UnitId@.
newtype PackageQualifier = PackageQualifier FastString

lookupUnitPackageQualifier :: HscEnv -> UnitId -> Maybe PackageQualifier
lookupUnitPackageQualifier env uid = home_unit_name <|> ext_unit_name
          where
            -- See Note [Package Qualified Imports]
            home_unit_name = PackageQualifier . mkFastString <$> (thisPackageName =<< home_unit_dflags env uid)
            ext_unit_name = const (PackageQualifier (unitIdFS uid)) <$> lookupUnitId (hsc_units env) uid

packageImportDecl :: PackageQualifier -> ModuleName -> ImportDecl GhcPs
packageImportDecl (PackageQualifier pkgName) mn =
  (GHC.simpleImportDecl $ mn)
    { ideclPkgQual = RawPkgQual
        StringLiteral
          { sl_st = NoSourceText
          , sl_fs = pkgName
          , sl_tc = Nothing
          }
    }
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


getTargetFileSummary ::
  HscEnv ->
  GHC.Target ->
  IO (Either GHC.DriverMessages GHC.ModSummary)
getTargetFileSummary hsc_env target
  | GHC.TargetFile file mb_phase <- targetId
  = do
    let offset_file = GHC.augmentByWorkingDirectory dflags file
    exists <- liftIO $ doesFileExist offset_file
    if exists || isJust maybe_buf
    then summariseFile hsc_env home_unit old_summary_map offset_file mb_phase
         maybe_buf
    else
      return $ Left $ GHC.singleMessage $
      GHC.mkPlainErrorMsgEnvelope noSrcSpan (GHC.DriverFileNotFound offset_file)
  | otherwise = error "FIXME"
  where
      old_summary_map = Map.empty
      GHC.Target {targetId, targetContents = maybe_buf, targetUnitId = uid} = target
      home_unit = ue_unitHomeUnit uid (hsc_unit_env hsc_env)
      dflags = homeUnitEnv_dflags (ue_findHomeUnitEnv uid (hsc_unit_env hsc_env))

compileModuleWithDepsInHpt ::
  GHC.Target ->
  Ghc (Maybe SourceError)
compileModuleWithDepsInHpt target@GHC.Target{targetUnitId = uid} = do
  hsc_env0 <- getSession
  let !old_active = hscActiveUnitId hsc_env0
  let !hsc_env = hscSetActiveUnitId uid hsc_env0
  ehmi <- liftIO $ try @SourceError $ do
    Right summary <- getTargetFileSummary hsc_env target
    result <- compileOne hsc_env (forceRecomp summary) 1 1 Nothing (GHC.HomeModLinkable Nothing Nothing)
    cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env) (hsc_tmpfs hsc_env) (ms_hspp_opts summary)
    pure result
  case ehmi of
   Left e -> do
     return $ Just e
   Right hmi -> do
    setSession . hscSetActiveUnitId old_active =<< liftIO (addDepsToHscEnv [hmi] hsc_env)
    return Nothing
  where
    -- This bypasses another recompilation check in 'compileOne'
    forceRecomp summary =
      summary {ms_hspp_opts = gopt_set (ms_hspp_opts summary) Opt_ForceRecomp}

addDepsToHscEnv :: [HomeModInfo] -> HscEnv -> IO HscEnv
addDepsToHscEnv deps hsc_env = do
  for_ deps $ \ dep -> hscInsertHPT dep hsc_env
  pure hsc_env

cleanCurrentModuleTempFilesMaybe :: MonadIO m => GHC.Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs

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

getPgmI :: DynFlags -> String
getPgmI df = toolSettings_pgm_i (toolSettings df)

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

-- | See Note [ UniqSupply is process global ]
initUniqSupplyIO :: IO ()
initUniqSupplyIO = GHC.initUniqSupply 0 1

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
