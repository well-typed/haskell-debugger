# Haskell Debugger

Status: **Work In Progress**

We are working on a first class debugger for Haskell.
It is still not ready for general consumption!

We will properly announce through the common channels the debugger when the
first major release is ready.

![CI badge](https://github.com/well-typed/haskell-debugger/actions/workflows/debugger.yaml/badge.svg) ![Hackage badge](https://img.shields.io/hackage/v/haskell-debugger.svg)

# Installation

Please find up to date installation instructions on the
[project homepage](https://well-typed.github.io/haskell-debugger/)!

> [!WARNING]
> `hdb` can currently be compiled with the 9.14 alpha pre-releases or with a nightly version
> The first release it will be compatible with is GHC 9.14.

To install and use the debugger, you need the executable `hdb`
and the VSCode extension [Haskell Debugger](https://marketplace.visualstudio.com/items?itemName=Well-Typed.haskell-debugger-extension).

Since `hdb` implements the [Debug Adapter Protocol
(DAP)](https://microsoft.github.io/debug-adapter-protocol/), it also supports
debugging with tools such as vim, neovim, or emacs -- as long as a DAP client is
installed and the `launch` arguments for `hdb` configured.

To run the debugger, the same version of GHC which compiled it needs to be in
PATH. Make sure the DAP client knows this. For instance, to launch VSCode with a specific GHC use:
```
PATH=/path/to/ghc-dir:$PATH code /path/to/proj
```

# Usage

To use the debugger in VSCode, select the debugger tab, select Haskell Debugger,
and create a `launch.json` file by clicking the debugger settings icon (next to
the green run button). Now, it is also supported to just Run a file which
contains a `main` function.

The `launch.json` file contains some settings about the debugger session here.
Namely:

| Setting | Description |
| --- | --- |
| `projectRoot`  | the full path to the project root. this is typically `${workspaceFolder}`, a value which is interpolated by the editor with the actual path                                           |
| `entryFile`    | the relative path from the project root to the file with the entry point for execution                                                                                                |
| `entryPoint`   | the name of the function that is called to start execution                                                                                                                            |
| `entryArgs`    | the arguments passed to the `entryPoint`. If the `entryPoint` is `main`, these arguments are passed as environment arguments (as in `getArgs`) rather than direct function arguments. |
| `extraGhcArgs` | additional flags to pass to the ghc invocation that loads the program for debugging.                                                                                                  |

Change them accordingly.

To run the debugger, simply hit the green run button.
See the Features section below for what is currently supported.

# Related Work

`hdb` is inspired by the original
[`haskell-debug-adapter`](https://github.com/phoityne/haskell-debug-adapter/) by @phoityne.

`hdb` improves on the original ideas implemented in
`haskell-debug-adapter` but makes them more robust by implementing the debugger
directly via the GHC API (similarly to HLS), rather than by communicating with a
custom `ghci` process.

We have been doing custom work on GHC to support debugging in a predictable,
robust, and more performant way. That is why `hdb` is only
compatible with the latest and greatest GHC. If you want to debug using an older
GHC version (9.12 and older), please check out `haskell-debug-adapter`.

To implement the Debug Adapter Protocol (DAP) server part, we are using the
[`dap`](https://hackage.haskell.org/package/dap-0.2.0.0) library by @dmjio.
`dap` is a framework for building language-agnostic DAP.

The `hdb` is transparently compatible with most projects (simple,
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
