#!/bin/bash
# Merges per-GHC-version build outputs for one distro/arch into the final
# release tarball. Mirrors HLS's build/bindist split so that adding a second
# GHC_VERSION later only means adding real multi-binary merge logic here --
# the job graph, actions, and entrypoint.sh dispatch don't change.
#
# Required env:
#   ARTIFACT    - ghcup-style arch/distro tag, e.g. x86_64-linux-deb10
#
# Expects out/${ARTIFACT}/<ghc-version>/hdb-<hdb-version>-ghcup-${ARTIFACT}.tar.gz
# to already be extracted (entrypoint.sh does this for the BINDIST stage).

set -euxo pipefail

: "${ARTIFACT:?ARTIFACT must be set}"

versions=(out/"${ARTIFACT}"/*/)
if [ "${#versions[@]}" -ne 1 ]; then
	echo "bindist.sh: expected exactly one GHC version under out/${ARTIFACT}, found ${#versions[@]} -- multi-GHC merging is not implemented yet" >&2
	exit 1
fi

tarballs=("${versions[0]}"hdb-*-ghcup-"${ARTIFACT}".tar.gz)
if [ "${#tarballs[@]}" -ne 1 ] || [ ! -f "${tarballs[0]}" ]; then
	echo "bindist.sh: expected exactly one hdb-*-ghcup-${ARTIFACT}.tar.gz in ${versions[0]}, found: ${tarballs[*]}" >&2
	exit 1
fi

# The BUILD stage already made the tarball with the final name.
cp "${tarballs[0]}" .
echo "Bindist for ${ARTIFACT}: $(basename "${tarballs[0]}")"
