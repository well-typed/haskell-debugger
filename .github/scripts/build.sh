#!/bin/bash
# Builds hdb inside one of the .github/actions/bindist-actions/action-*
# containers and packages it via scripts/mk-bindist.sh.

set -euxo pipefail

: "${GHC_VERSION:?GHC_VERSION must be set}"
: "${GHCUP_CHANNEL:?GHCUP_CHANNEL must be set}"
: "${HDB_VERSION:?HDB_VERSION must be set}"
: "${ARTIFACT:?ARTIFACT must be set}"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_MINIMAL=1
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

export PATH="$HOME/.ghcup/bin:$PATH"

ghcup install cabal --set
ghcup --url-source "${GHCUP_CHANNEL}" install ghc "${GHC_VERSION}" --set

cabal update
cabal build hdb --enable-executable-dynamic

STAGE_DIR="out/${ARTIFACT}/${GHC_VERSION}"
mkdir -p "${STAGE_DIR}"
HDB_VERSION="${HDB_VERSION}" ARCH="${ARTIFACT}" OUT_DIR=dist-bin bash scripts/mk-bindist.sh
mv "hdb-${HDB_VERSION}-${ARTIFACT}.tar.gz" "${STAGE_DIR}/"
tar cf "out-${ARTIFACT}-${GHC_VERSION}.tar" out/
