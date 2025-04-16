# GHC Debugger

Status: **Work In Progress**

We are working on a first class debugger for Haskell.
It is still not ready for general consumption!

We will properly announce through the common channels the debugger when the
first release is ready.


# Usage

```
PATH="/path/to/head/ghc:$PATH" cabal install haskell-debugger-server
```

Then, install the VSCode extension `haskell-debugger-extension` that you can
build with `nix-build` in the extension directory, dragging the result into the
VSCode extensions tab, and you're set.

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
