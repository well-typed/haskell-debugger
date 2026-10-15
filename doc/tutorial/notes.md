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
- GHC debugging grand finale?
- debug console, with breakpoint bindings, imports
- bytecode artefacts accellerate loading.

## Plan
- Quick demo of stopping and stepping through something, some basics.
- Setting it up with vscode. (Mention any editor with DAP support should work, e.g. neovim and emacs).
  - install with --enable-executable-dynamic
  - launch.json
- "Solve" a bug in the quick demo showing more basic features (different buttons).
- Tackle a larger project/bug, maybe in HLS? showing hie.yaml

## Things to explain
In no particular order yet.
- VSCode commands:
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

- Breakpoints are inserted early in the compilation pipeline, so they can be matched to source positions.
  However this also means optimization passes, e.g. inlining, can move them around.
- lazily inspecting vars in the variables pane.
- can run native code too, but can only stop at bytecode compiled with -fbreak-points
- logging breakpoints
- conditional breakpoints
  - count
  - boolean cond
- breakpoints can be changed while stopped.

## Open Questions
- does stack trace show for stopped at exceptions now?
- are breakpoints for StepOut inserted at Core or during bytecode generation?

