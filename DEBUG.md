# Debugging hdb+GHC

## prerequisites

- GHC source tree at ./ghc with a ghc/compiler that allows Cabal-3.16.*
  - For 9.14 cherry-pick b5508f2cb4e46aedb8f6232c1d42bd697e3dd2f0 
    - See https://gitlab.haskell.org/trac-Saizan/ghc/-/commits/debug-9.14
  - Gone through early stages of prep and build so `./ghc/_build/stageBoot/bin/` is populated. 
- Installed ghc of same version and flavour as GHC source tree being debugged.
- Installed hdb with executable-dynamic: True.

## How To
- Uncomment the bios cradle in the hdb's hie.yaml and comment the others.
  - The bios cradle can also be left in place to have an HLS session with both ghc/compiler and hdb loaded.
- Setup the final debuggee with a cradle that won't use package dbs setup by HIE.
  - See Limitations
  - See ./scripts/cabal-repl-bios.sh for a script that can be used as a bios cradle for this, uses cabal repl.
- See .vscode/launch.json for how to start a Debug session of hdb.

### Limitations

#### Segfaults
We are getting segfaults in the debugger hdb process, and this happens before running the debuggee. It's been useful to set `verbosity = 3` in `runDebugger` to have a rough idea of what can trigger the segfault.

Current workarounds are:
- Avoid loading haskell-debugger-view modules with debuggee hdb.
  - This is covered by `-DDEBUG_WITH_GHC` in `cabal.project.debug`
  - Thankfully we can still use the HDV modules in the debugger hdb.
  - Segfault happens somewhere in CoreTidy step.
- Use a final debuggee (the one the debuggee hdb will be debugging) that doesn't depend on a package-db built through hie-bios. I suspect something is going wrong reading the `package.cache` binaries.
  - See ./scripts/cabal-repl-bios.sh for a script that can be used as a bios cradle for this, uses cabal repl.
  - Segfault happens during `initializing unit database`.


Possible reasons for segfaults:
  - We point the debugee ghc to the libdir of the installed ghc, and there's a lot of platform/build specifics there.
  - The debugee *interpreted* `ghc` library will interact with the *compiled* RTS that's handling the process where we execute the debugee.
    - It is linked against the installed rts library, but there might be more subtle coupling to address?
  - The debugee `ghc` library interacts with the installed hdb external interpreter process which is linked against the installed ghc library.

TODOs:
  - Try debugger hdb linked with recompiled ghc library, i.e. built with `--project-file=cabal.project.debug --ghc-options=-UDEBUG_WITH_GHC`.
  - Try with an installed ghc that was locally built, as a dynamic executable, to rule out system library differences.

#### Not quite integrated with Hadrian

- We explicitly add `./ghc/_build/stageBoot/bin/` to the $PATH so we can run .ghc/compiler/Setup.hs. 
  - Maybe that's a way to ask hadrian for the right env?

- Ideally we would have a `multi:hdb` target that extends the existing `multi:ghc` one, however that would require:
  - Extending hadrian to handle packages with multiple components (haskell-debugger.cabal is one of those).
  - Setting up all the 133 transitive dependencies of hdb as user packages too.

TODO: Would `multi:hdb` help with the segfaults? What would the mechanism be?
