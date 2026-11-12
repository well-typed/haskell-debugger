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

dump_ghcup_logs() {
	for f in "$HOME"/.ghcup/logs/*.log; do
		echo "----- $f -----" >&2
		cat "$f" >&2
	done
}
trap dump_ghcup_logs ERR

ghcup install cabal --set
ghcup --url-source "${GHCUP_CHANNEL}" install ghc "${GHC_VERSION}" --set

trap - ERR

cabal update
cabal build hdb --enable-executable-dynamic

STAGE_DIR="out/${ARTIFACT}/${GHC_VERSION}"
mkdir -p "${STAGE_DIR}"
# Add the "-ghcup" infix to the version, as the native jobs in bindists.yaml do.
# mk-bindist.sh then puts the infix in the directory inside the tarball too, which
# is the directory that ghcup's dlSubdir points to.
HDB_VERSION="${HDB_VERSION}-ghcup" ARCH="${ARTIFACT}" OUT_DIR=dist-bin bash scripts/mk-bindist.sh
mv "hdb-${HDB_VERSION}-ghcup-${ARTIFACT}.tar.gz" "${STAGE_DIR}/"
tar cf "out-${ARTIFACT}-${GHC_VERSION}.tar" out/
