{-# LANGUAGE DerivingVia, StandaloneDeriving, ViewPatterns, ImpredicativeTypes #-}
module GHC.Debugger.View.Class where

-- | The representation of the value for some variable on the debugger
data VarValue = VarValue
  { -- | The value to display inline for this variable
    varValue      :: String

    -- | Can this variable further be expanded (s.t. @'debugFields'@ is not null?)
  , varExpandable :: Bool
  }
  deriving (Show, Read)

-- | The representation for fields of a value which is expandable in the debugger
newtype VarFields = VarFields
  { varFields :: [(String, VarFieldValue)]
  }

-- | A box for subfields of a value.
--
-- Used to construct the debug-view list of fields one gets from expanding a datatype.
-- See, for instance, the @DebugView (a, b)@ instance for an example of how it is used.
--
-- The boxed value is returned as is and can be further forced or expanded by
-- the debugger, using either the existing @'DebugView'@ instance for the
-- existential @a@ (the instance is found at runtime), or the generic runtime
-- term inspection mechanisms otherwise.
data VarFieldValue = forall a. VarFieldValue a

-- | Custom handling of debug terms (e.g. in the variables pane, or when
-- inspecting a lazy variable)
class DebugView a where

  -- | Compute the representation of a variable with the given value.
  --
  -- INVARIANT: this method should only called on values which are already in
  -- WHNF, never thunks.
  --
  -- That said, this method is responsible for determining how much it is
  -- forced when displaying it inline as a variable.
  --
  -- For instance, for @String@, @a@ will be fully forced to display the entire
  -- string in one go rather than as a linked list of @'Char'@.
  debugValue :: a -> VarValue

  -- | Compute the fields to display when expanding a value of type @a@.
  --
  -- This method should only be called to get the fields if the corresponding
  -- @'VarValue'@ has @'varExpandable' = True@.
  debugFields :: a -> VarFields

--------------------------------------------------------------------------------

-- | Boring types scaffolding.
--
-- Meant to be used like:
--
-- @
-- deriving via (BoringTy Int) instance (DebugView Int)
-- @
--
-- to derive a 'DebugView' for a type whose terms should always be fully forced
-- and displayed whole rather than as parts.
--
-- A boring type is one for which we don't care about the structure and would
-- rather see "whole" when being inspected. Strings and literals are a good
-- example, because it's more useful to see the string value than it is to see
-- a linked list of characters where each has to be forced individually.
newtype BoringTy a = BoringTy a

instance Show a => DebugView (BoringTy a) where
  debugValue (BoringTy x) = VarValue (show x) False
  debugFields _           = VarFields []

deriving via BoringTy Int     instance DebugView Int
deriving via BoringTy Word    instance DebugView Word
deriving via BoringTy Double  instance DebugView Double
deriving via BoringTy Float   instance DebugView Float
deriving via BoringTy Integer instance DebugView Integer
deriving via BoringTy Char    instance DebugView Char
deriving via BoringTy String  instance DebugView String

instance DebugView (a, b) where
  debugValue _ = VarValue "( , )" True
  debugFields (x, y) = VarFields
    [ ("fst", VarFieldValue x)
    , ("snd", VarFieldValue y) ]

-- instance DebugView T.Text where
--   debugValue  t = VarValue (show (T.unpack t)) False
--   debugFields _ = VarFields []
--
-- instance DebugView BS.ByteString where
--   debugValue  t = VarValue (show (T.unpack (T.decodeUtf8 t))) False
--   debugFields _ = VarFields []
--
-- instance DebugView (IM.IntMap a) where
--   debugValue _ = VarValue "IntMap" True
--   debugFields im = VarFields
--     [ (show k, VarFieldValue v)
--     | (k, v) <- IM.toList im
--     ]
--
-- instance Show k => DebugView (M.Map k a) where
--   debugValue _ = VarValue "Map" True
--   debugFields m = VarFields
--     [ (show k, VarFieldValue v)
--     | (k, v) <- M.toList m
--     ]

--------------------------------------------------------------------------------
-- * (Internal) Wrappers required to call `evalStmt` on methods more easily
--------------------------------------------------------------------------------

-- | Wrapper to make evaluating from debugger easier
data VarValueIO = VarValueIO
  { varValueIO :: IO String
  , varExpandableIO :: Bool
  }

debugValueIOWrapper :: DebugView a => a -> IO [VarValueIO]
debugValueIOWrapper x = case debugValue x of
  VarValue str b ->
    pure [VarValueIO (pure str) b]

newtype VarFieldsIO = VarFieldsIO
  { varFieldsIO :: [(IO String, VarFieldValue)]
  }

debugFieldsIOWrapper :: DebugView a => a -> IO [VarFieldsIO]
debugFieldsIOWrapper x = case debugFields x of
  VarFields fls ->
    pure [VarFieldsIO [ (pure fl_s, b) | (fl_s, b) <- fls]]
