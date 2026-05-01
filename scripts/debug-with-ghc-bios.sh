#!/bin/bash

ALLOWED_DIRS=(ghc/compiler haskell-debugger hdb hdb-dap test)

ARG=$(realpath ${HIE_BIOS_ARG:-$1})

is_allowed() {
    for dir in "${ALLOWED_DIRS[@]}"; do
	realdir=$(realpath $dir)
        [[ "$ARG" == "$realdir"/* || "$ARG" == "$realdir" ]] && return 0
    done
    return 1
}

export PATH="$PWD/ghc/_build/stageBoot/bin/:$PATH"

# The is_allowed check speeds things up when opening a stray file.
if is_allowed "$ARG"; then
  $(dirname $0)/cabal-repl-bios.sh cabal.project.debug all
fi
