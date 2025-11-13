{-# LANGUAGE TemplateHaskell #-}

-- | Built-in units and modules
module GHC.Debugger.Session.Builtin
  ( -- * Built-in mods
    debuggerViewBuiltinMods
  , debuggerViewInstancesMods
  , debuggerViewClassModName, debuggerViewClassContents

    -- * In memory unit
  , hsDebuggerViewInMemoryUnitId
  , addInMemoryHsDebuggerViewUnit
  , makeInMemoryHsDebuggerViewTarget

  -- Note:
  -- Don't export instances mods individually to make sure we get warnings if
  -- we add new modules but forget to put any part of them there.
  )
  where

import Data.FileEmbed
import Data.Function
import Data.Time

import GHC
import GHC.Unit
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Data.StringBuffer
import qualified GHC.Unit.Home.Graph as HUG
import qualified GHC.Unit.Home.PackageTable as HPT
import qualified GHC.Unit.State as State

--------------------------------------------------------------------------------
-- * Built-in Modules
--------------------------------------------------------------------------------

-- | The set of modules to load from @haskell-debugger-view@.
-- NOTE: This list should always be kept up to date with the modules listed in
-- @exposed-modules@ in @haskell-debugger-view@ to make sure all (possibly
-- orphan) instances are loaded and available.
debuggerViewBuiltinMods :: [(ModuleName, StringBuffer)]
debuggerViewBuiltinMods = (debuggerViewClassModName, debuggerViewClassContents):debuggerViewInstancesMods

-- | The modules which provide orphan instances for types defined in external packages.
-- We will try to load each of these modules separately.
debuggerViewInstancesMods :: [(ModuleName, StringBuffer)]
debuggerViewInstancesMods =
  [ (debuggerViewContainersModName, debuggerViewContainersContents)
  ]

-- | GHC.Debugger.View.Class
debuggerViewClassModName :: ModuleName
debuggerViewClassModName = mkModuleName "GHC.Debugger.View.Class"

-- | GHC.Debugger.View.Containers
debuggerViewContainersModName :: ModuleName
debuggerViewContainersModName = mkModuleName "GHC.Debugger.View.Containers"

--------------------------------------------------------------------------------
-- * In memory haskell-debugger-view
--------------------------------------------------------------------------------

-- | The fixed unit-id (@haskell-debugger-view-in-memory@) for when we load the haskell-debugger-view modules in memory
hsDebuggerViewInMemoryUnitId :: UnitId
hsDebuggerViewInMemoryUnitId = toUnitId $ stringToUnit "haskell-debugger-view-in-memory"

-- | Create a unit @haskell-debugger-view@ which uses in-memory files for the modules
--  and add it to the HUG
addInMemoryHsDebuggerViewUnit
  :: GhcMonad m
  => [UnitId] -- ^ The unit-ids from the transitive dependencies closure of the user-given targets
  -> DynFlags -- ^ Dynflags resulting from first downsweep of user given targets
  -> m ()
addInMemoryHsDebuggerViewUnit base_uids initialDynFlags = do
  let imhdv_dflags = initialDynFlags
        { homeUnitId_ = hsDebuggerViewInMemoryUnitId
        , importPaths = []
        , packageFlags =
          [ ExposePackage
                  ("-package-id " ++ unitIdString unitId)
                  (UnitIdArg $ RealUnit (Definite unitId))
                  (ModRenaming True [])
          | unitId <- base_uids
          , unitId /= rtsUnitId
          , unitId /= ghcInternalUnitId
          ]
        }
        & setGeneralFlag' Opt_HideAllPackages
  hsc_env <- getSession
  (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits (hsc_logger hsc_env) imhdv_dflags Nothing mempty
  updated_dflags <- liftIO $ updatePlatformConstants imhdv_dflags mconstants
  emptyHpt <- liftIO HPT.emptyHomePackageTable
  modifySession $ \env ->
    env
      -- Inserts the in-memory hdv unit
      & hscUpdateHUG (\hug ->
          let hdv_hue = HUG.HomeUnitEnv
               { HUG.homeUnitEnv_units = unit_state
               , HUG.homeUnitEnv_unit_dbs = Just dbs
               , HUG.homeUnitEnv_dflags = updated_dflags
               , HUG.homeUnitEnv_hpt = emptyHpt
               , HUG.homeUnitEnv_home_unit = Just home_unit
               }
           in HUG.unitEnv_insert hsDebuggerViewInMemoryUnitId hdv_hue hug
      )

-- | Make an in-memory 'GHC.Target' for a @haskell-debugger-view@ built-in
-- module from the module name and contents
makeInMemoryHsDebuggerViewTarget :: ModuleName -> StringBuffer -> IO GHC.Target
makeInMemoryHsDebuggerViewTarget modName sb = do
    time <- getCurrentTime
    let mkTarget mn contents = GHC.Target
          { targetId = GHC.TargetFile ("in-memory:" ++ moduleNameString mn) Nothing
          , targetAllowObjCode = False
          , GHC.targetUnitId = hsDebuggerViewInMemoryUnitId
          , GHC.targetContents = Just (contents, time)
          }
    return $ mkTarget modName sb

--------------------------------------------------------------------------------
-- * In memory module contents
--------------------------------------------------------------------------------

-- | The contents of GHC.Debugger.View.Class in memory
debuggerViewClassContents :: StringBuffer
debuggerViewClassContents = stringToStringBuffer $(embedStringFile "haskell-debugger-view/src/GHC/Debugger/View/Class.hs")

-- | The contents of GHC.Debugger.View.Containers in memory
debuggerViewContainersContents :: StringBuffer
debuggerViewContainersContents = stringToStringBuffer $(embedStringFile "haskell-debugger-view/src/GHC/Debugger/View/Containers.hs")
