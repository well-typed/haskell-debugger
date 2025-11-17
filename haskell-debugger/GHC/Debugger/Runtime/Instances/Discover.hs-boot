module GHC.Debugger.Runtime.Instances.Discover where

import GHC.Core.Map.Type

type RuntimeInstancesCache = TypeMap (Maybe DebugViewInstance)
data DebugViewInstance
emptyRuntimeInstancesCache :: RuntimeInstancesCache
