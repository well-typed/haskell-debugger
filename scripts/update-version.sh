#!/bin/sh

printf "Enter next version: "
read NEW_VERSION

if [ -z "$NEW_VERSION" ]; then
  echo "Error: Version cannot be empty"
  exit 1
fi

# Update versions
sed -i.bkp -E "s/(^version:\s*)[0-9\.]*(\s*$)/\1$NEW_VERSION.0\2/" haskell-debugger.cabal
sed -i.bkp -E "s/(^\s*version = \").*(\"\;$)/\1$NEW_VERSION\2/" vscode-extension/default.nix
sed -i.bkp -E "s/(^\s*\"version\":\s*\").*(\"\,$)/\1$NEW_VERSION\2/" vscode-extension/package.json

