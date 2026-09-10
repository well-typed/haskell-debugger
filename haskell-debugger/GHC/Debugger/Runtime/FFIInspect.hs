{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module GHC.Debugger.Runtime.FFIInspect where
import GHC.Base (StackSnapshot#)
import GHC.Stack.CloneStack
import GHC.Exts.Heap qualified as GHC
import GHC.Exts.Heap.Closures qualified as GHC

foreign import ccall unsafe "stack.h" bco_args_offset :: StackSnapshot# -> Word -> Int
foreign import ccall unsafe "stack.h" stack_bco_frame_selftest :: IO Bool


-- | Takes a frame location for a continuation RET_BCO frame.
--   Returns the offset of `bcoArgs` in the AP_STACK object that you'd get if stopping at the beginning of the continuation BCO.
bcoArgsOffset :: StackSnapshot -> Int -> Maybe Word
bcoArgsOffset (StackSnapshot ss) frame_offset =
  case bco_args_offset ss (fromIntegral frame_offset) of
    i | i < 0 -> Nothing
      | otherwise -> Just $ fromIntegral i

getClosureData :: GHC.StgStackClosure -> IO GHC.Closure
getClosureData = GHC.getClosureData
