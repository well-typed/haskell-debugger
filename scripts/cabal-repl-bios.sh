#!/bin/bash

PROJECT_FILE=$1

shift

cabal repl -v0 -O0 --project-file=${PROJECT_FILE} --builddir=cabal-repl-bios --keep-temp-files --with-repl=echo $@ | sed 's/^--interactive //' | tr '[:blank:]' '\n' | tee $HIE_BIOS_OUTPUT
