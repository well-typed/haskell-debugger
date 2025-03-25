# Usage

```
# Make sure both ghc-debugger and haskell-debugger-server are built
cabal build exe:ghc-debugger
export PATH="$(dirname $(cabal list-bin exe:ghc-debugger)):$PATH"
cabal run haskell-debugger-server
```

To avoid recompilation-related errors, use a script with the above to run the
server. This should save you some major headaches caused by forgetting to update
the ghc-debugger executable.

Then, install the VSCode extension `haskell-debugger-extension` with `npm run
package`, dragging the result into the VSCode extensions tab, and you're set.

# Features

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
