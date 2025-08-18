# GHC Debugger

Status: **Work In Progress**

We are working on a first class debugger for Haskell.
It is still not ready for general consumption!

We will properly announce through the common channels the debugger when the
first major release is ready.

![CI badge](https://github.com/well-typed/ghc-debugger/actions/workflows/debugger.yaml/badge.svg) ![Hackage badge](https://img.shields.io/hackage/v/ghc-debugger.svg)

# Installation

> [!WARNING]
> `ghc-debug-adapter` is only supported by the latest nightly GHC version.
> The first release it will be compatible with is GHC 9.14.

To install and use the GHC debugger, you need the executable `ghc-debug-adapter`
and the VSCode extension `haskell-debugger-extension`.

Since `ghc-debug-adapter` implements the [Debug Adapter Protocol
(DAP)](https://microsoft.github.io/debug-adapter-protocol/), it also supports
debugging with tools such as vim, neovim, or emacs -- as long as a DAP client is
installed and the `launch` arguments for `ghc-debug-adapter` configured.

To build, install, and run `ghc-debug-adapter` you currently need a nightly
version of GHC in PATH. You can get one using
[GHCup](https://ghcup.readthedocs.io/en/latest/guide/), or [building from source](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation):
```
ghcup config add-release-channel https://ghc.gitlab.haskell.org/ghcup-metadata/ghcup-nightlies-0.0.7.yaml
ghcup install ghc latest-nightly 
PATH=$(dirname $(ghcup whereis ghc latest-nightly)):$PATH cabal install ghc-debugger:ghc-debug-adapter --enable-executable-dynamic --allow-newer=ghc-bignum,containers,time,ghc
```

To run the debugger, the same nightly version of GHC needs to be in PATH. Make
sure the DAP client knows this. For instance, to launch VSCode use:
```
PATH=$(dirname $(ghcup whereis ghc latest-nightly)):$PATH code /path/to/proj
```
Currently, to install the debugger extension, download the `.vsix` file from the
GitHub release artifacts and drag and drop it to the extensions side panel. In
the future we will release it on the marketplace.

# Usage

To use the debugger in VSCode, select the debugger tab, select Haskell Debugger,
and create a `launch.json` file by clicking the debugger settings icon (next to
the green run button).

The `launch.json` file contains some settings about the debugger session here.
Namely:

| Setting | Description |
| --- | --- |
| `entryFile`    | the relative path from the project root to the file with the entry point for execution                                                                                                |
| `entryPoint`   | the name of the function that is called to start execution                                                                                                                            |
| `entryArgs`    | the arguments passed to the `entryPoint`. If the `entryPoint` is `main`, these arguments are passed as environment arguments (as in `getArgs`) rather than direct function arguments. |
| `extraGhcArgs` | additional flags to pass to the ghc invocation that loads the program for debugging.                                                                                                  |

Change them accordingly.

To run the debugger, simply hit the green run button.
See the Features section below for what is currently supported.

Note: Listing global variables is only supported in GHC versions newer than May 6, 2025

# Related Work

`ghc-debug-adapter` is inspired by the original
[`haskell-debug-adapter`](https://github.com/phoityne/haskell-debug-adapter/) by @phoityne.

`ghc-debug-adapter` improves on the original ideas implemented in
`haskell-debug-adapter` but makes them more robust by implementing the debugger
directly via the GHC API (similarly to HLS), rather than by communicating with a
custom `ghci` process.

We have been doing custom work on GHC to support debugging in a predictable,
robust, and more performant way. That is why `ghc-debug-adapter` is only
compatible with the latest and greatest GHC. If you want to debug using an older
GHC version (9.12 and older), please check out `haskell-debug-adapter`.

To implement the Debug Adapter Protocol (DAP) server part, we are using the
[`dap`](https://hackage.haskell.org/package/dap-0.2.0.0) library by @dmjio.
`dap` is a framework for building language-agnostic DAP.

The `ghc-debug-adapter` is transparently compatible with most projects (simple,
Cabal, Stack, custom `hie.yaml`) because
it uses [`hie-bios`](https://github.com/haskell/hie-bios) to figure out the
right flags to prepare the GHC session with.


# Features

Many not listed! Here are a few things:

## Stepping

- [x] Continue (resume execution forward)
- [x] Next (step within local function)
- [x] Step into (single step to next immediate tick)
- [x] Step out (execute until end of function and break after the call)

### In Reverse

- [ ] Local step backwards (ie reverse of Next)
- [ ] Single step backwards (ie reverse of Step into)
- [ ] Continue backwards (resume execution backwards until a breakpoint is hit)

## Breakpoints

- [x] Module breakpoints
- [x] Function breakpoints
- [x] Exception breakpoints
- [ ] Data breakpoints
- [ ] Instruction breakpoints

### Conditionals
- [ ] Conditional breakpoints     (breakpoint is hit only if condition is satisfied)
- [ ] Hit conditional breakpoints (stop after N number of hits)

# Building from source

To build `ghc-debug-adapter`:
```
cabal build -w /path/to/recent/ghc exe:ghc-debug-adapter
```

To build the VSCode extension
```
cd vscode-extension
nix-build
```

## Testing

```
cd test/integration-tests
nix-shell
make GHC=/path/to/recent/ghc \
     DEBUGGER=$(cd ../.. && cabal list-bin -w /path/to/recent/ghc exe:ghc-debug-adapter)
```
