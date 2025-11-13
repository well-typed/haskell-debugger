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
import Data.Maybe
import Data.Time

import GHC
import GHC.Unit
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Data.StringBuffer
import qualified GHC.Unit.Home.Graph as HUG
import qualified GHC.Unit.Home.PackageTable as HPT

import GHC.Debugger.Session

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
debuggerViewInstancesMods = [(debuggerViewContainersModName, debuggerViewContainersContents)]

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
  => DynFlags -- ^ Initial dynflags
  -> m ()
addInMemoryHsDebuggerViewUnit initialDynFlags = do
  let imhdv_dflags = initialDynFlags
        { homeUnitId_ = hsDebuggerViewInMemoryUnitId
        , importPaths = []
        , packageFlags = []
        }
  emptyHpt <- liftIO HPT.emptyHomePackageTable
  modifySession $ \env ->
    env
      -- Inserts the in-memory hdv unit
      & hscUpdateHUG (\hug ->
          let idebugger_hue =
                fromJust $ HUG.unitEnv_lookup_maybe interactiveGhcDebuggerUnitId hug
              hdv_hue = HUG.HomeUnitEnv
               { HUG.homeUnitEnv_units = HUG.homeUnitEnv_units idebugger_hue
               , HUG.homeUnitEnv_unit_dbs = Nothing
               , HUG.homeUnitEnv_dflags = imhdv_dflags
               , HUG.homeUnitEnv_hpt = emptyHpt
               , HUG.homeUnitEnv_home_unit = Just (DefiniteHomeUnit hsDebuggerViewInMemoryUnitId Nothing)
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
