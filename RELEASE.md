# Release process

- Note: the scripts depend on GNU sed, use it (`nix-shell -p binutils`)

- Make sure the package version is a new version by running
  `./scripts/update-version.sh` and typing a version without the patch number,
  like `0.12.0`

- Make sure the CHANGELOG is up to date

- Make sure the version of haskell-debugger-view that is supported by the new
  debugger version is checked at runtime (the check already exists, just make
  sure it is up to date). A new version of the haskell-debugger-view package
  will be released by the last step at the same time, if needed.


- Merge all those changes from a PR into master and then run
  `./scripts/release.sh` from master.
  This script will validate a few things and then add a version tag.

  A dry-run of the release pipeline is triggered by pushing the tag `git push --tags`.
  
  Review the generated artefacts uploaded as a github draft release.
  
  To actually do the release explicitly trigger `workflow_dispatch` on the
  `release` workflow with `publish: true`
