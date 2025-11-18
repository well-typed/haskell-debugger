{-# LANGUAGE DerivingVia, StandaloneDeriving, ViewPatterns, ImpredicativeTypes #-}
-- | The home of customizability for visualizing variables and values with @haskell-debugger@
module GHC.Debugger.View.Class
  (
    -- * Writing custom debug visualizations
    --
    -- | The entry point for custom visualizations is 'DebugView'.
    -- There are two axis of configuration:
    --
    -- 1. What to display inline in front of the variable name and whether it
    -- is expandable
    --
    -- 2. What fields are displayed when the value is expanded and what are
    -- their corresponding values
    --
    -- The former is answered by 'debugValue' / 'VarValue' and the latter by
    -- 'debugFields' / 'VarFields'.
    DebugView(..)

  , VarValue(..)
  , VarFields(..)
  , VarFieldValue(..)

  -- * Utilities
  --
  -- | These can make it easier to write your own custom instances.
  -- We also use them for the built-in custom instances.
  , BoringTy(..)

  -- * The internals
  --
  -- | These are used by @haskell-debugger@ when invoking these instances at
  -- runtime and reconstructing the result from the heap.
  --
  -- They should never be used by a user looking to write custom visualizations.
  , VarValueIO(..)
  , debugValueIOWrapper
  , VarFieldsIO(..)
  , debugFieldsIOWrapper
  )
  where

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
