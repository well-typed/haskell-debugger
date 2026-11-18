# HDB Tutorial

## Features
- setting breakpoints by location or function name
- multi-component support via hie.yaml
- CLI support (though will change soon, maybe I can get the script ready for the demo?)
- DAP support (main focus I guess)
- continue, stepIn, stepOut, (any I forgot?) I think it's good to show each of these and how to use them.
- scopes
- stack frames
- lazy variable inspection
- custom DebugView support
- what we have working of multi-threaded
- GHC debugging grand finale? slow without bytecode artifacts
- debug console, with breakpoint bindings, imports
- bytecode artefacts accellerate loading.

## Plan
- Quick demo of stopping and stepping through something, some basics.
- Setting it up with vscode. (Mention any editor with DAP support should work, e.g. neovim and emacs).
  - install with --enable-executable-dynamic
  - launch.json
- "Solve" a bug in the quick demo showing more basic features (different buttons).
- Tackle a larger project/bug, maybe in HLS? showing hie.yaml
- Agda? could work on backported bc artifacts 9.14 and nightly

## Things to explain

In no particular order yet.
- highlight VSCode native vs. haskell specific
- VSCode commands: on the parser example
  - Continue: keeps executing until the next active breakpoint.
  - StepIn: stepping follows lazy evaluation order
    - `let y = 10*1000; print (f y)` will first step into the body of `f`, then when
      `y` is demanded we jump back to the `let`.
  - StepOver: does not mean "skip the highlighted expression", it means "go to next breakpoint in this declaration".
    - The DAP request is actually "Next", which is fairly vague, but applied to
      statements in an imperative language could be sensibly named "StepOver".
    - WARN: if there's no more breakpoints to hit in this declaration then it's
      equivalent to Continue.
  - StepOut: stop at the first stack frame outside of the current toplevel declaration, if it has a breakpoint. 
    - WARN: if no such frame exists it's equivalent to Continue.
  - Stop: terminates session
  - Restart: same as stop+start

The various commands are better explained in code with straight function calls
rather than parser combinators. So demos/stepout/Main.hs is more appropriate.

Here's an example sequence of steps that let you showcase all the steps. It's
also at the bottom of the file.

-- Script:
-- breakpoint on applyDiscount (qty v) (sum (prices v))
-- StepOver: (qty v)                      -- StepOver is "steplocal" stays in this decl
-- StepInto: validate o                   -- StepInto: next thing evaluated. StepOver would have gone to `sum (prices v)`, can explain?
-- StepInto: case o of ..                 -- inside validate
-- StepOver: | qty o <= 0                 -- next thing same decl, but we don't care so..
-- StepOut: (qty v) again                 -- where `v` was demanded first.
-- StepOut: if q >= 3                     -- what demanded (qty v) in the first place
-- StepOver: subtotal - subtotal `div` 10 -- next thing same decl
-- StepInto: (sum (prices v))             -- evaluating subtotal
-- StepInto: prices                       -- evaluates the projection
-- StepInto: subtotal `div` 10            -- subtraction demands this next
-- Any: done.                             -- nothing more in bytecode to execute.
  
- Nightly is much faster because of custom commands I presume.
- Breakpoints are inserted early in the compilation pipeline, so they can be matched to source positions.
  However this also means optimization passes, e.g. inlining, can move them around.
- lazily inspecting vars in the variables pane.
  - DebugView show if parser accept
  - can run native code too, but can only stop at bytecode compiled with -fbreak-points
- logging breakpoints
- conditional breakpoints
  - count
  - boolean cond
- breakpoints can be changed while stopped.
- talk about coming multithreaded
- talk about bytecode artifacts

## Open Questions
- does stack trace show for stopped at exceptions now?
- are breakpoints for StepOut inserted at Core or during bytecode generation?

