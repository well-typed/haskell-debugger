# Revision history for haskell-debugger

## 0.11.0.0 -- 2026-01-05

* Introduce but don't yet expose infrastructure for debugging multi-threaded
  programs and constructing better callstacks.
* Introduce `RemoteExpr`: A DSL for evaluation on the remote process.
    * This allows us to cache remote expressions at a much greater granularity
      which improves performance and memory usage significantly in the long term.
* Improves logging to Debug Console
* Augment `haskell-debugger-version`, version `0.2.0.0` with
    * This release brings the `Program` DSL which allows the debuggee to be
      queried from a custom instance, making it possible to implement
      visualisations which rely on e.g. evaluatedness of a term.
        * See the `DebugView` instance for `[a]` for an example of a more
          complex instance.
    * `haskell-debugger-0.11.0.0` is only compatible with
      `haskell-debugger-view-0.2.0.0` and will abort if unsupported
      `haskell-debugger-view` versions are found in the dependencies closure
* Bump `dap` minimum version to `0.3.1`
* Various fixes and code improvements

## 0.10.1.0 -- 2025-11-21

* Fixes critical bug which prevented debugger from being used with non-boot
  dependencies!

## 0.10.0.0 -- 2025-11-18

* Adds Custom Debug Visualisations!
    * The value inspection internals were refactored to always try first to use a
      custom visualization and fallback to the general-case visualization which
      mimics the heap representation of a value.
    * We ship a handful of custom visualisations for base types like `String`,
      `Int`, etc, and for types from a few core packages: `Text`, `ByteString`, `Map` and `IntMap`.
    * The user can add custom visualizations for the desired types by
      implementing an instance for `DebugView` from `haskell-debugger-view`
        * At runtime, `hdb` will pick up the `DebugView` instances in scope and use them when possible.
    * When the package dependency closure includes `haskell-debugger-view`, we
      will use that unit specifically. When it is not in the dependencies, we
      will load a built-in version in memory.
* Evaluate requests now return a structured and expandable response, akin to the variables pane
    * To show the whole value inline one can recover the previous behavior by
      calling `show` on the value to display.
    * This makes it possible to have structured results in the "Watch" pane.
* Adds support for conditional breakpoints and hit count breakpoints
* Bug fixes in variable inspection
* Bug fixes in multiple-home-units

## 0.9.0.0 -- 2025-10-13

### Main changes

* Run a proxy program with `runInTerminal` to allow stdin via the terminal process, if the client supports it.

### Bug fixes

* Fix bug where build failures were reported in a pop-up rather than stderr
* Fix crashes panicking with `findUnitIdOfEntryFile`
* Fix cli bug by use absolute entryFile path
* Fix bug caused by not canonicalizing special target and root
* Fix in variable expansion (expand `Term`s iteratively as the user expands the tree)

## 0.8.0.0 -- 2025-09-19

* Allow defaults for all settings except `entryFile` and return a proper error in that case
* Fix bug that crashed debugger when attempting to load newtype constructor closure
* Fix bug that broke displaying newtype variables
* (Vscode) Allow the debugger to be run without a launch.json file
* Don't display functions as a forceable thunk, instead just the type
* Add `--version` flag

## 0.7.0.0 -- 2025-09-10

* Fix line buffering of debuggee output (thus, stepping through a print line, will indeed print it to the console now).
    * In fact, this was only caused by not building with `--enable-executable-dynamic`

## 0.6.0.0 -- 2025-09-10

* Improve unit handling and fix running when `main` functions exist across different units
    * Add interactive home unit id
    * The entry file determines which of the `main`s to run.
* Add proper option parsing
* Add preview version of `hdb cli` interactive mode, as an alternative to DAP server
    * Experimental and incomplete
* Fix: output uncaught exceptions to stderr
* Fix: output eval completed result to console

## 0.5.0.0 -- 2025-08-26

* Compatibility with GHC 9.14
* Add support for stepping out of functions as a tech preview
* Use implicit cradle discovery to better support multiple configurations,
  mirroring HLS (and thus providing a more similar experience)
* Query the GHC runtime version via hie-bios, now honoring e.g. `with-compiler:
  ...` in `cabal.project` to fetch the right GHC version
* Rename package from `ghc-debugger` to `haskell-debugger`, and
  `ghc-debug-adapter` to `hdb`, to be consistent with other tools and
  ecosystems and to avoid ambiguity with `ghc-debug` (program heap analysis library and
  tool)
* Use cache directories for `hdb` to have faster startup times. This will only
  be enabled for compilers supporting the upcoming `.gbc` (compiled bytecode
  artifact) files.

## 0.4.0.0 -- 2025-06-27

* Add support for debugging multiple home units (MHU)

## 0.3.0.0 -- 2025-06-07

* Critical fixes for variables inspection

## 0.2.0.0 -- 2025-05-13

* Significantly improves variable inspection and expansion commands.

## 0.1.0.0 -- 2025-05-08

* First version. Released on an unsuspecting world.
