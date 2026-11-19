#!/bin/bash
# Entrypoint for the .github/actions/bindist-actions/action-* Docker containers.
# Dispatches on $STAGE. Only the BUILD stage installs the container's toolchain.
# The BINDIST stage unpacks the BUILD stage's output and copies a tarball, which
# only needs bash, tar and cp, and every image has them.

set -exo pipefail

export DEBIAN_FRONTEND=noninteractive

case "$STAGE" in
	BUILD)
		bash -c "$PKG_INSTALL_CMD $PKG_TOOLS"
		bash .github/scripts/build.sh
		;;
	BINDIST)
		for t in out-*.tar; do tar xf "$t"; done
		bash .github/scripts/bindist.sh
		;;
	*)
		echo "entrypoint.sh: unknown STAGE: $STAGE" >&2
		exit 1
		;;
esac
