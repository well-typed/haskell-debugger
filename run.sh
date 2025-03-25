#!/bin/sh

cabal build exe:ghc-debugger
export PATH="$(dirname $(cabal list-bin exe:ghc-debugger)):$PATH"
cabal run haskell-debugger-server
