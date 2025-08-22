#!/bin/sh

# The most part of a release is automatically done by GitHub actions.
# That includes (only if CI is successful):
#   * Releasing the package (and docs) on Hackage
#   * Creating the release on GitHub
#       * Copying the .vsix extension to the release artifacts
#
# The release action is triggered by a new tag matching "v${version}".
# The "manual" part of the release is done by this script.
# That includes:
#   * Validating the extension version matches the Haskell package version
#   * Creating the commit with the release keyword for that version
#   * Tagging the release
#
# Note: the version must be manually updated by using ./scripts/update-version.sh
#   * before running the release script, to the desired release version
#   * after the release commit, to the intermediate unreleased version
#
# Pushing is manual to be deliberate. Pushing will trigger a release!

set -e

if [ ! -f "haskell-debugger.cabal" ]; then
  echo "Error: haskell-debugger.cabal not found. Please run this script from the project root."
  exit 1
fi

PKG_VERSION=$(sed -nE "s/^version:\s*([0-9\.]*).*$/\1/p" haskell-debugger.cabal)
EXTENSION_NIX_VERSION=$(sed -nE "s/^\s*version = \"(.*)\"\;$/\1/p" vscode-extension/default.nix)
EXTENSION_PKG_VERSION=$(sed -nE "s/^\s*\"version\":\s*\"(.*)\"\,$/\1/p" vscode-extension/package.json)

if test -z "$PKG_VERSION"; then
  echo "Error: PKG_VERSION sed expression returned an empty string."
  echo "Maybe you should try GNU sed: nix-shell -p binutils."
  echo "Currently using: $(which sed)"
  exit 1
fi

# Check versions match
if [ "$PKG_VERSION" != "$EXTENSION_NIX_VERSION.0" ] || [ "$PKG_VERSION" != "$EXTENSION_PKG_VERSION.0" ]; then
  echo "Error: Version mismatch"
  echo "PKG_VERSION           = $PKG_VERSION"
  echo "EXTENSION_NIX_VERSION = $EXTENSION_NIX_VERSION"
  echo "EXTENSION_PKG_VERSION = $EXTENSION_PKG_VERSION"
  exit 1
fi

# Nothing should be staged.
# Git diff --quiet returns 0 if diff is empty
if ! git diff --staged --quiet; then
  echo "Error: You have staged changes. Please commit or unstage them before proceeding."
  exit 1
fi

# Do the release commit and tag it
git commit --allow-empty -m "Release: $PKG_VERSION"
git tag "v$PKG_VERSION" -m "Release: $PKG_VERSION"

echo "Please don't forget to push with --tags"
