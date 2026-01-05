# Revision history for haskell-debugger-view

## 0.2.0.0 -- 2026-01-05

* Add support for more complex debug visualizations using the Program abstraction
    * API change: `debugValue` and `debugFields` now return `Program`-wrapped
      values, rather than `VarFields/VarValue` directly.
* Introduce new functions `isThunk` and `ifP` for conditional debugging behavior
* Enhance list visualization to show up to 50 forced elements
* Add improved handling for lazy values in debug views
* Update documentation and examples for custom DebugView instances

## 0.1.0.0 -- 2025-11-18

* First version. Released on an unsuspecting world.
