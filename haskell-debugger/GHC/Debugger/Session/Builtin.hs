{-# LANGUAGE TemplateHaskell #-}

-- | Built-in units and modules
module GHC.Debugger.Session.Builtin where

import Data.Time
import Data.FileEmbed

import GHC
import GHC.Unit
import GHC.Data.StringBuffer

--------------------------------------------------------------------------------
-- * Built-in Module names
--------------------------------------------------------------------------------

-- | GHC.Debugger.View.Class
debuggerViewClassModName :: ModuleName
debuggerViewClassModName = mkModuleName "GHC.Debugger.View.Class"

--------------------------------------------------------------------------------
-- * In memory haskell-debugger-view
--------------------------------------------------------------------------------

-- | The fixed unit-id (@haskell-debugger-view-in-memory@) for when we load the haskell-debugger-view modules in memory
hsDebuggerViewInMemoryUnitId :: UnitId
hsDebuggerViewInMemoryUnitId = toUnitId $ stringToUnit "haskell-debugger-view-in-memory"

-- | Create a unit @haskell-debugger-view@ which uses in-memory files for the modules
makeInMemoryHsDebuggerViewUnit
  :: DynFlags                    -- ^ Initial dynflags
  -> IO (DynFlags, [GHC.Target]) -- ^ The dynflags and targets of the unit
makeInMemoryHsDebuggerViewUnit initialDynFlags = do
    let hdvDynFlags = initialDynFlags
          { homeUnitId_ = hsDebuggerViewInMemoryUnitId
          , importPaths = []
          , packageFlags = []
          }
    time <- getCurrentTime
    let mkTarget mn contents = GHC.Target
          { targetId = GHC.TargetFile ("in-memory:" ++ moduleNameString mn) Nothing
          , targetAllowObjCode = False
          , GHC.targetUnitId = hsDebuggerViewInMemoryUnitId
          , GHC.targetContents = Just (contents, time)
          }
    return
      ( hdvDynFlags
      , [ mkTarget debuggerViewClassModName debuggerViewClassContents
        ]
      )

--------------------------------------------------------------------------------
-- * In memory module contents
--------------------------------------------------------------------------------

-- | The contents of GHC.Debugger.View.Class in memory
debuggerViewClassContents :: StringBuffer
debuggerViewClassContents = stringToStringBuffer $(embedStringFile "haskell-debugger-view/src/GHC/Debugger/View/Class.hs")
