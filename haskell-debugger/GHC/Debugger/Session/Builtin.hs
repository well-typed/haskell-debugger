{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Built-in units and modules
module GHC.Debugger.Session.Builtin
  ( -- * HsDebuggerView unit
    -- ** Modules
    debuggerViewBuiltinMods
  , debuggerViewInstancesMods
  -- Note:
  -- Don't export instances mods individually to make sure we get warnings if
  -- we add new modules but forget to put any part of them there.
  , debuggerViewClassModName, debuggerViewClassContents
    -- ** In memory unit
  , hsDebuggerViewInMemoryUnitId
  , addInMemoryHsDebuggerViewUnit

    -- * DebuggerInternal unit
    -- ** Modules
  , debuggerRuntimeInternalModName, debuggerRuntimeInternalContents
  , debuggerRuntimeInternalModule
    -- ** In memory unit
  , debuggerInternalUnitId
  , debuggerRuntimeInternalUnit
  , addInMemoryDebuggerInternalUnit
    -- ** Utils
  , lookupNoPrintConstant
  , runInternal

#if !MIN_VERSION_ghc(9,14,2)
  -- * FFIInspect unit
  -- ** Modules
  , debuggerRuntimeFFIInspectModName, debuggerRuntimeFFIInspectContents
  -- ** In memory unit
  , addInMemoryFFIInspectUnit
  , hsDebuggerFFIInspectUnitId
#endif
  -- * Helpers
  , makeInMemoryTarget
  )
  where

import Data.FileEmbed
import Data.Function
import Data.Time
#if !MIN_VERSION_ghc(10,1,0)
import Data.Maybe
import qualified Data.Foldable as Foldable
#endif

import GHC
import GHC.Unit
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Data.StringBuffer
import qualified GHC.Unit.Home.Graph as HUG
import qualified GHC.Unit.Home.PackageTable as HPT
import qualified GHC.Unit.State as State
import GHC.Data.FastString (unpackFS)
import Data.Coerce
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt
import GHC.Runtime.Context (InteractiveContext(..), emptyInteractiveContext)
import Control.Monad.Catch (finally)
import GHC.Iface.Env (lookupNameCache)
import GHC.Types.Name (mkVarOcc)

--------------------------------------------------------------------------------
-- * Built-in Modules
--------------------------------------------------------------------------------

-- | The set of modules to load from @haskell-debugger-view@.
-- NOTE: This list should always be kept up to date with the modules listed in
-- @exposed-modules@ in @haskell-debugger-view@ to make sure all (possibly
-- orphan) instances are loaded and available.
debuggerViewBuiltinMods :: [(ModuleName, StringBuffer)]
debuggerViewBuiltinMods = (debuggerViewClassModName, debuggerViewClassContents):map (\(a,b,_) -> (a,b)) debuggerViewInstancesMods

-- | The modules which provide orphan instances for types defined in external packages.
-- We will try to load each of these modules separately.
debuggerViewInstancesMods :: [(ModuleName, StringBuffer, String {- package name -})]
debuggerViewInstancesMods =
  [ ( debuggerViewContainersModName
    , debuggerViewContainersContents
    , "containers"
    )
  , ( debuggerViewTextModName
    , debuggerViewTextContents
    , "text"
    )
  , ( debuggerViewByteStringModName
    , debuggerViewByteStringContents
    , "bytestring"
    )
  ]

-- | GHC.Debugger.View.Class
debuggerViewClassModName :: ModuleName
debuggerViewClassModName = mkModuleName "GHC.Debugger.View.Class"

-- | GHC.Debugger.View.Containers
debuggerViewContainersModName :: ModuleName
debuggerViewContainersModName = mkModuleName "GHC.Debugger.View.Containers"

-- | GHC.Debugger.View.Text
debuggerViewTextModName :: ModuleName
debuggerViewTextModName = mkModuleName "GHC.Debugger.View.Text"

-- | GHC.Debugger.View.ByteString
debuggerViewByteStringModName :: ModuleName
debuggerViewByteStringModName = mkModuleName "GHC.Debugger.View.ByteString"

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
addInMemoryHsDebuggerViewUnit = addInMemoryUnit
  hsDebuggerViewInMemoryUnitId
  (PackageName "haskell-debugger-view")

#if !MIN_VERSION_ghc(9,14,2)
-- | The fixed unit-id (@haskell-debugger-ffi-inspect@) used to load @GHC.Debugger.Runtime.FFIInspect@ in the debuggee when we can't have custom ghci-serv commands.
hsDebuggerFFIInspectUnitId :: UnitId
hsDebuggerFFIInspectUnitId = stringToUnitId "haskell-debugger-ffi-inspect"

addInMemoryFFIInspectUnit :: GhcMonad m => [UnitId] -> DynFlags -> m UnitId
addInMemoryFFIInspectUnit deps dflags = do
  addInMemoryUnit
    hsDebuggerFFIInspectUnitId
    (coerce hsDebuggerFFIInspectUnitId)
    deps dflags
  return hsDebuggerFFIInspectUnitId
#endif

-- run internal here serves to overwrite certain flags while executing the
-- internal "evalWrapper" computation which is not relevant to the user.
runInternal :: GhcMonad m => m a -> m a
runInternal m = withSavedSession $ do
  modifySession $
    -- The new imports are checked against the old ones: GHC attempts to scan
    -- them for orphan instances, and crashes if those modules are not
    -- accessible from the new active unit.
    --
    -- We empty anything to do with things defined interactively too.
    emptyIC .
    hscSetActiveUnitId debuggerInternalUnitId
  setContext [IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"]
  m
  where
    withSavedSession act = do
      s <- getSession
      act `finally` setSession s
    emptyIC env = case hsc_IC env of
      InteractiveContext{..} ->
        env {hsc_IC = (emptyInteractiveContext ic_dflags)
              { ic_mod_index = ic_mod_index
              , ic_int_print = ic_int_print
              , ic_monad = ic_monad
              }}

debuggerInternalUnitId :: UnitId
debuggerInternalUnitId = stringToUnitId "haskell-debugger-internal"

addInMemoryDebuggerInternalUnit :: (MonadFail m, GhcMonad m) => DynFlags -> m ()
addInMemoryDebuggerInternalUnit dflags = do
  us <- hsc_units <$> getSession
  Just deps' <- pure $ mapM (lookupPackageName us . PackageName) ["ghc-heap","ghci"]

  let deps = baseUnitId dflags
#if !MIN_VERSION_ghc(9,14,2)
        : hsDebuggerFFIInspectUnitId
#endif
        : deps'
  addInMemoryUnit
    debuggerInternalUnitId
    (coerce debuggerInternalUnitId)
    deps $
    dflags
         { -- Running GHCi's internal expression is incompatible with -XSafe.
            -- We temporarily disable any Safe Haskell settings while running
            -- GHCi internal expressions. (see #12509)
          safeHaskell = GHC.Sf_None,
            -- Disable dumping of any data during evaluation of GHCi's internal
            -- expressions. (#17500)
          dumpFlags = EnumSet.empty
        }
          -- RebindableSyntax can wreak havoc with GHCi in several ways
            -- (see #13385 and #14342 for examples), so we temporarily
            -- disable it too.
            `xopt_unset` LangExt.RebindableSyntax
            -- We heavily depend on -fimplicit-import-qualified to compile expr
            -- with fully qualified names without imports.
            `gopt_set` Opt_ImplicitImportQualified


  return ()

-- | Gives @Name@ of a strict @noPrintConstant :: a -> IO ()@
lookupNoPrintConstant :: Ghc Name
lookupNoPrintConstant = do
    hsc_env <- getSession
    let debuggerInternalUnit = RealUnit (Definite debuggerInternalUnitId)
    liftIO $ lookupNameCache (hsc_NC hsc_env) (mkModule debuggerInternalUnit debuggerRuntimeInternalModName)
       (mkVarOcc "noPrintConstant")

addInMemoryUnit :: GhcMonad m
  => UnitId      -- ^ The unit-id for the unit to add
  -> PackageName -- ^ The package name for the unit to add
  -> [UnitId]    -- ^ The unit-ids for dependencies
  -> DynFlags    -- ^ Dynflags to base the unit on.
  -> m ()
addInMemoryUnit uid (PackageName pkgName) base_uids initialDynFlags = do
  let imhdv_dflags = initialDynFlags
        { homeUnitId_ = uid
        , importPaths = []
        , packageFlags =
          [ ExposePackage
                  (unitIdString unitId)
                  (UnitIdArg $ RealUnit (Definite unitId))
                  (ModRenaming True [])
          | unitId <- base_uids
          , unitId /= rtsUnitId
          , unitId /= ghcInternalUnitId
          ]
        , thisPackageName = Just $ unpackFS pkgName
        }
        & flip gopt_unset Opt_HideAllPackages
        & flip gopt_unset Opt_InsertBreakpoints
#if MIN_VERSION_ghc(9,14,2)
        -- In memory modules should not write .hi nor .gbc files.
        & flip gopt_unset Opt_WriteByteCode
        & flip gopt_unset Opt_WriteInterface
#endif
  hsc_env <- getSession
#if MIN_VERSION_ghc(10,1,0)
  (unit_state,home_unit,mconstants) <- liftIO $ State.initUnits (hsc_logger hsc_env) imhdv_dflags (hscUIC hsc_env) $ HUG.allUnits $ hsc_HUG $ hsc_env
#else
  let cached_unit_dbs = concat . catMaybes . fmap HUG.homeUnitEnv_unit_dbs $ Foldable.toList (hsc_HUG hsc_env)
  (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits (hsc_logger hsc_env) imhdv_dflags (Just cached_unit_dbs) $ HUG.allUnits $ hsc_HUG $ hsc_env
#endif
  updated_dflags <- liftIO $ updatePlatformConstants imhdv_dflags mconstants
  emptyHpt <- liftIO HPT.emptyHomePackageTable
  modifySession $ \env ->
    env
      -- Inserts the in-memory hdv unit
      & hscUpdateHUG (\hug ->
          let hdv_hue = HUG.HomeUnitEnv
               { HUG.homeUnitEnv_units = unit_state
#if !MIN_VERSION_ghc(10,1,0)
               , HUG.homeUnitEnv_unit_dbs = Just dbs
#endif
               , HUG.homeUnitEnv_dflags = updated_dflags
               , HUG.homeUnitEnv_hpt = emptyHpt
               , HUG.homeUnitEnv_home_unit = Just home_unit
               }
           in HUG.unitEnv_insert uid hdv_hue hug
      )


-- | Make an in-memory 'GHC.Target' for a module from the module name and contents
makeInMemoryTarget :: UnitId -> ModuleName -> StringBuffer -> IO GHC.Target
makeInMemoryTarget uid modName sb = do
    time <- getCurrentTime
    let mkTarget mn contents = GHC.Target
          { targetId = GHC.TargetFile ("in-memory:" ++ moduleNameString mn) Nothing
          , targetAllowObjCode = False
          , GHC.targetUnitId = uid
          , GHC.targetContents = Just (contents, time)
          }
    return $ mkTarget modName sb


{- Note [debuggerInternal unit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a few cases we want to compile and run debugger code on the interpreter, e.g.

- define constant for `setInteractivePrintName`
- format and print messages for logging breakpoints
- Specialize `evalWrapper` to provided arguments

and more uses if you include .Legacy.

There is however a challenge: the normally active unit is
`interactiveGhcDebugger`, which depends on the debuggee units, so we can't be
sure how module names are resolved: if the intended module is shadowed by
another package you might just get a "Not in scope" error. Extensions like
`RebindableSyntax` or `Overloaded*` also cause problems, as listed in the
comments to `runInternal` in ghci's codebase.

As a defensive measure we add an in-memory unit, debuggerInternalUnit, dedicated to
running "internal" code, which only depends on boot packages we need.

The unit is depended upon by interactiveGhcDebugger, and exposes the module
`GHC.Debugger.Runtime.Internal` where various aliases or helpers that are needed
at runtime are defined. The module is also compiled as part of the
haskell-debugger package, so it can be used from custom commands if needed.

The aliases are helpful when we have to evaluate expressions that mix
debuggee/user code and internal code, like `logMessageExpression`, because there's
less of a chance that e.g. `GHC.Debugger.Runtime.Internal.concat` would clash
compared to `Prelude.concat`, since there are custom preludes out there.

Moreover we define our own version of `runInteral` which temporarily sets
`debuggerInternalUnit` as the active unit, reducing the possible interactions.

Reccommendations for runtime code:
  - See if it can be made a custom command first.
  - Refer only to functions via `GHC.Debugger.Runtime.Internal` not any other modules.
  - Prefer plain function application rather than syntactic sugar (even list or tuple syntax counts as sugar).
  - Define an helper in GHC.Debugger.Runtime.Internal rather than evaluate a larger expression.
  - If compiling exclusively internal code, use `runInternal`.

For .Legacy the reccommendation is relaxed to the use of runInternal, as it
should be sufficient and avoids polishing a module we want to get rid of.
-}


--------------------------------------------------------------------------------
-- * In memory module contents
--------------------------------------------------------------------------------

-- | The contents of GHC.Debugger.View.Class in memory
debuggerViewClassContents :: StringBuffer
debuggerViewClassContents = stringToStringBuffer $(embedStringFile =<< makeRelativeToProject "haskell-debugger-view/src/GHC/Debugger/View/Class.hs")

-- | The contents of GHC.Debugger.View.Containers in memory
debuggerViewContainersContents :: StringBuffer
debuggerViewContainersContents = stringToStringBuffer $(embedStringFile =<< makeRelativeToProject "haskell-debugger-view/src/GHC/Debugger/View/Containers.hs")

-- | GHC.Debugger.View.Text
debuggerViewTextContents :: StringBuffer
debuggerViewTextContents = stringToStringBuffer $(embedStringFile =<< makeRelativeToProject "haskell-debugger-view/src/GHC/Debugger/View/Text.hs")

-- | GHC.Debugger.View.ByteString
debuggerViewByteStringContents :: StringBuffer
debuggerViewByteStringContents = stringToStringBuffer $(embedStringFile =<< makeRelativeToProject "haskell-debugger-view/src/GHC/Debugger/View/ByteString.hs")

#if !MIN_VERSION_ghc(9,14,2)
debuggerRuntimeFFIInspectModName :: ModuleName
debuggerRuntimeFFIInspectModName = mkModuleName "GHC.Debugger.Runtime.FFIInspect"

-- | The contents of GHC.Debugger.Runtime.FFIInspect in memory
debuggerRuntimeFFIInspectContents :: StringBuffer
debuggerRuntimeFFIInspectContents = stringToStringBuffer $(embedStringFile =<< makeRelativeToProject "haskell-debugger/GHC/Debugger/Runtime/FFIInspect.hs")
#endif

debuggerRuntimeInternalModName :: ModuleName
debuggerRuntimeInternalModName = mkModuleName "GHC.Debugger.Runtime.Internal"

-- | The contents of GHC.Debugger.Runtime.FFIInspect in memory
debuggerRuntimeInternalContents :: StringBuffer
debuggerRuntimeInternalContents = stringToStringBuffer $(embedStringFile =<< makeRelativeToProject "haskell-debugger/GHC/Debugger/Runtime/Internal.hs")

debuggerRuntimeInternalModule :: Module
debuggerRuntimeInternalModule = mkModule debuggerRuntimeInternalUnit debuggerRuntimeInternalModName

debuggerRuntimeInternalUnit :: Unit
debuggerRuntimeInternalUnit = RealUnit (Definite debuggerInternalUnitId)

