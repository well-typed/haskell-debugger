# GHC Debugger

Status: **Work In Progress**

We are working on a first class debugger for Haskell.
It is still not ready for general consumption!

We will properly announce through the common channels the debugger when the
first major release is ready.

![CI badge](https://github.com/well-typed/ghc-debugger/actions/workflows/debugger.yaml/badge.svg)

# Installation

To install and use the GHC debugger, you need the executable `ghc-debug-adapter`
and the VSCode extension `haskell-debugger-extension` (or you can configure the
launch arguments similarly in a DAP client for (n)vim or emacs).

To build, install, and run `ghc-debug-adapter` you currently need a nightly
version of GHC in PATH. You can get one using
[GHCup](https://ghcup.readthedocs.io/en/latest/guide/), or [building from source](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation):
```
ghcup config add-release-channel https://ghc.gitlab.haskell.org/ghcup-metadata/ghcup-nightlies-0.0.7.yaml
ghcup install ghc latest-nightly 
PATH=$(dir $(ghcup whereis ghc latest-nightly)):$PATH cabal install exe:ghc-debug-adapter --allow-newer=ghc-bignum,ghc,base,template-haskell,ghc-boot-th,ghci,containers,time
```

To run the debugger, the same nightly version of GHC needs to be in PATH. Make
sure the DAP client knows this. For instance, to launch VSCode use:
```
PATH=$(dir $(ghcup whereis ghc latest-nightly)):$PATH code /path/to/proj
```
Currently, to install the debugger extension, download the `.vsix` file from the
GitHub release artifacts and drag and drop it to the extensions side panel. In
the future we will release it on the marketplace.

Then, select the debugger tab, select Haskell Debugger, and create a
`launch.json` file by clicking the debugger settings icon (next to the green run
button). You can change some settings about the debugger session (such as the
entry file) here.

Note: Listing global variables is only supported in GHC versions newer than May 7, 2025

# Features

Many not listed! Here are a few things:

## Stepping

- [x] Continue (resume execution forward)
- [x] Next (step within local function)
- [x] Step into (single step to next immediate tick)
- [ ] Step out (execute until end of function and break after the call)

### In Reverse

- [ ] Local step backwards (ie reverse of Next)
- [ ] Single step backwards (ie reverse of Step into)
- [ ] Continue backwards (resume execution backwards until a breakpoint is hit)

## Breakpoints

- [x] Module breakpoints
- [x] Function breakpoints
- [x] Exception breakpoints
- [ ] Data breakpoints
- [ ] Instruction breakpoints

### Conditionals
- [ ] Conditional breakpoints     (breakpoint is hit only if condition is satisfied)
- [ ] Hit conditional breakpoints (stop after N number of hits)
