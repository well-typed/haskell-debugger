#!/bin/bash
# Entrypoint for the .github/actions/bindist-actions/action-* Docker containers.
# Installs the container's toolchain, then dispatches on $STAGE.

set -exo pipefail

bash -c "$PKG_INSTALL_CMD $PKG_TOOLS"

case "$STAGE" in
	BUILD)
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
