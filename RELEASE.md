# Release process

- Note: the scripts depend on GNU sed, use it (`nix-shell -p binutils`)

- Make sure the package version is a new version by running
  `./scripts/update-version.sh` and typing a version without the patch number,
  like `0.12.0`

- Make sure the CHANGELOG is up to date

- Make sure the haskell-debugger-view package on hackage is the latest (upload it manually if not)

- Make sure the version of haskell-debugger-view that is supported by the new
  debugger version is checked at runtime (the check already exists, just make
  sure it is up to date)

- Merge all those changes from a PR into master and then run
  `./scripts/release.sh` from master.
  This script will validate a few things and then add a version tag.

  The release pipeline is triggered by pushing the tag `git push --tags`
