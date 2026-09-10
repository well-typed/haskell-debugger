#!/usr/bin/env bash
# Build a self-contained "bindist" tarball for the hdb executable:
# Required env: HDB_VERSION, ARCH (asset arch tag, e.g. linux-x86_64), OUT_DIR.

set -euo pipefail

: "${HDB_VERSION:?HDB_VERSION must be set}"
: "${ARCH:?ARCH must be set}"
: "${OUT_DIR:?OUT_DIR must be set}"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BINDIST_NAME="hdb-${HDB_VERSION}-${ARCH}"
BINDIST_DIR="${OUT_DIR}/${BINDIST_NAME}"

rm -rf "${BINDIST_DIR}"
mkdir -p "${BINDIST_DIR}/bin" "${BINDIST_DIR}/lib"

GHC_VERSION="$(ghc --numeric-version)"

BIN="$(cabal list-bin hdb)"
cp "${BIN}" "${BINDIST_DIR}/bin/hdb"

case "$(uname -s)" in
	Darwin) strip "${BINDIST_DIR}/bin/hdb" ;;
	*)      strip -s "${BINDIST_DIR}/bin/hdb" ;;
esac

# Bundle hdb's non-boot dynamic dependencies. GHC's boot libraries live in the
# GHC installation's own libdir (matched at runtime by the wrapper), while
# hdb's project/Hackage dependencies live in the cabal store -- copy those.
STORE_DIR="$(cabal path --store-dir)"
GHC_STORE_DIR="$(find "${STORE_DIR}" -maxdepth 1 -type d -name "ghc-${GHC_VERSION}*" | head -n1)"
if [ -z "${GHC_STORE_DIR}" ]; then
	echo "mk-bindist.sh: could not find cabal store dir for GHC ${GHC_VERSION} under ${STORE_DIR}" >&2
	exit 1
fi

case "$(uname -s)" in
	Darwin) DLL_GLOB="*.dylib" ;;
	*)      DLL_GLOB="*.so" ;;
esac
find "${GHC_STORE_DIR}" -type f -name "${DLL_GLOB}" -exec cp {} "${BINDIST_DIR}/lib/" \;

# Rewrite the executable's rpath to find the bundled lib/ dir relative to
# itself, instead of the CI runner's (ephemeral) absolute store path.
case "$(uname -s)" in
	Darwin)
		install_name_tool -add_rpath "@executable_path/../lib" "${BINDIST_DIR}/bin/hdb"
		;;
	*)
		patchelf --force-rpath --set-rpath '$ORIGIN/../lib' "${BINDIST_DIR}/bin/hdb"
		;;
esac

BOOT_PKGS="$(ghc-pkg --global list --simple-output)"
ABI_HASHES="$(for dep in ${BOOT_PKGS} ; do printf "%s:" "${dep}" && ghc-pkg field "${dep}" abi --simple-output ; done | tr '\n' ' ' | xargs)"

sed \
	-e "s#@@EXE_DIR@@#bin#" \
	-e "s#@@EXE_NAME@@#hdb#" \
	-e "s#@@GHC_VERSION@@#${GHC_VERSION}#" \
	-e "s#@@BOOT_PKGS@@#${BOOT_PKGS}#" \
	-e "s#@@ABI_HASHES@@#${ABI_HASHES}#" \
	"${ROOT_DIR}/bindist/wrapper.in" > "${BINDIST_DIR}/hdb"
chmod 755 "${BINDIST_DIR}/hdb"

tar -C "${OUT_DIR}" -czf "hdb-${HDB_VERSION}-${ARCH}.tar.gz" "${BINDIST_NAME}"

echo "Built hdb-${HDB_VERSION}-${ARCH}.tar.gz"
