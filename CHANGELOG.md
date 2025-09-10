# Revision history for haskell-debugger

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
