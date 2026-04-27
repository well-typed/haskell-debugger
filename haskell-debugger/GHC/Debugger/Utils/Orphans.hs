{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Debugger.Utils.Orphans () where

import GHC.Debugger.View.Class
import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Debugger.Utils (showModule)

instance DebugView FastString where
  debugValue  t = simpleValue (unpackFS t) False
  debugFields _ = pure (VarFields [])

instance DebugView Module where
  debugValue  t = simpleValue (showModule t) False
  debugFields _ = pure (VarFields [])

instance DebugView (ModuleEnv a) where
  debugValue m = simpleValue "ModuleEnv" (not $ isEmptyModuleEnv m)
  debugFields m = pure $ VarFields
    [ (showModule k, VarFieldValue v)
    | (k, v) <- moduleEnvToList m
    ]
