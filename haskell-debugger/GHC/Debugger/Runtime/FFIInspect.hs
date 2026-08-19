{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module GHC.Debugger.Runtime.FFIInspect where
import GHC.Base (StackSnapshot#)
import GHC.Stack.CloneStack

foreign import ccall unsafe "stack.h" bco_args_offset :: StackSnapshot# -> Word -> Int
foreign import ccall unsafe "stack.h" stack_bco_frame_selftest :: IO Bool


-- | Takes a frame location for a continuation RET_BCO frame.
--   Returns the offset of `bcoArgs` in the AP_STACK object that you'd get if stopping at the beginning of the continuation BCO.
bcoArgsOffset :: StackSnapshot -> Word -> Maybe Word
bcoArgsOffset (StackSnapshot ss) frame_offset =
  case bco_args_offset ss frame_offset of
    i | i < 0 -> Nothing
      | otherwise -> Just $ fromIntegral i