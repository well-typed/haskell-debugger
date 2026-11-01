#!/usr/bin/env bash
# Check that an hdb bindist tarball can be installed and that `hdb` starts.
#
# Usage: test-install.sh <extract|ghcup> <bindist.tar.gz> <hdb-version>
#
# What "works" means: `hdb --version` exits successfully and prints the version.
# That requires the `hdb` wrapper script to find the real binary (bin/hdb) and
# its libraries (lib/), and to find the GHC that hdb was built with.
#
# Modes (two ways a user can install the bindist):
#
#   extract  Install by hand: unpack the tarball and put a symlink to its `hdb`
#            wrapper in a directory on PATH.
#
#   ghcup    Install with ghcup. ghcup only knows about tools listed in a
#            release channel, so we write a temporary channel file that points
#            at the local tarball, then run `ghcup install haskell-debugger`.
#            ghcup puts a symlink to the wrapper in ~/.ghcup/bin.
#
# Requirements:
#   - Both modes: the GHC that hdb was built with must be on PATH.
#   - ghcup mode: ghcup must be on PATH. It installs into the real ghcup
#     directory (or $GHCUP_INSTALL_BASE_PREFIX/.ghcup if set), so on a
#     machine you care about, clean up afterwards with:
#       ghcup rm haskell-debugger <hdb-version>

set -euo pipefail

if [ "$#" -ne 3 ]; then
	echo "usage: $0 <extract|ghcup> <bindist.tar.gz> <hdb-version>" >&2
	exit 2
fi

MODE="$1"
TARBALL="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
HDB_VERSION="$3"

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "${WORK_DIR}"' EXIT

# Print the SHA-256 of a file (sha256sum on Linux, shasum on macOS). ghcup
# requires this hash in the channel file and verifies it on download.
sha256() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | cut -d' ' -f1
	else
		shasum -a 256 "$1" | cut -d' ' -f1
	fi
}

# Run `<hdb> --version` and fail unless it prints "Haskell Debugger" and the
# expected version. $1 is the path to run, e.g. a symlink on PATH.
smoke_test() {
	local hdb="$1"
	echo "Running: ${hdb} --version"
	local out
	out="$("${hdb}" --version)"
	echo "${out}"
	case "${out}" in
		*"Haskell Debugger"*"${HDB_VERSION}"*) ;;
		*)
			echo "unexpected output from '${hdb} --version'" >&2
			exit 1
			;;
	esac
}

case "${MODE}" in
	extract)
		mkdir -p "${WORK_DIR}/unpacked" "${WORK_DIR}/path"
		tar -xzf "${TARBALL}" -C "${WORK_DIR}/unpacked"
		TOP_DIR="$(find "${WORK_DIR}/unpacked" -mindepth 1 -maxdepth 1 -type d | sed -n 1p)"
		# Check 1: run the wrapper in place. This checks the tarball itself
		# (hdb, bin/hdb and lib/ are all present and a matching GHC is found).
		smoke_test "${TOP_DIR}/hdb"
		# Check 2: run the wrapper through a symlink, as it is when linked onto
		# PATH (by hand or by ghcup). A wrapper that cannot follow symlinks
		# fails here, with "<symlink dir>/bin/hdb: No such file".
		ln -s "${TOP_DIR}/hdb" "${WORK_DIR}/path/hdb"
		smoke_test "${WORK_DIR}/path/hdb"
		;;

	ghcup)
		command -v ghcup >/dev/null 2>&1 || { echo "ghcup not found on PATH" >&2; exit 1; }

		case "$(uname -s)" in
			Darwin) OS_KEY="Darwin" ;;
			Linux)  OS_KEY="Linux_UnknownLinux" ;;
			*) echo "unsupported OS: $(uname -s)" >&2; exit 1 ;;
		esac
		case "$(uname -m)" in
			arm64|aarch64) ARCH_KEY="A_ARM64" ;;
			x86_64)        ARCH_KEY="A_64" ;;
			*) echo "unsupported architecture: $(uname -m)" >&2; exit 1 ;;
		esac

		# The tarball's single top-level directory. (`sed -n 1p`, not `head -n1`:
		# GNU tar fails with "write error" when head closes the pipe early.)
		TOP_DIR_NAME="$(tar -tzf "${TARBALL}" | sed -n 1p | cut -d/ -f1)"
		CHANNEL="${WORK_DIR}/channel.yaml"

		# Write the temporary channel file. It has a single entry for just this
		# machine's OS and architecture, pointing at the local tarball.
		# The tool details and `dlInstallSpec` (which files ghcup installs and
		# which symlinks it creates) must match the haskell-debugger entry in
		# https://github.com/haskell/ghcup-metadata (ghcup-3rdparty-*.yaml),
		# otherwise this test does not test what users get.
		cat > "${CHANNEL}" <<EOF
ghcupDownloads:
  haskell-debugger:
    toolDetails:
      toolHomepage: "https://github.com/well-typed/haskell-debugger"
      toolRepository: "https://github.com/well-typed/haskell-debugger"
      toolDescription: "A step-through debugger for Haskell"
      toolAuthor: "Well-Typed"
      toolLicense: "BSD-3"
    toolVersions:
      ${HDB_VERSION}:
        viTags:
          - Latest
        viArch:
          ${ARCH_KEY}:
            ${OS_KEY}:
              unknown_versioning:
                dlUri: file://${TARBALL}
                dlHash: $(sha256 "${TARBALL}")
                dlSubdir: ${TOP_DIR_NAME}
                dlInstallSpec:
                  exeRules:
                    - installSource: "hdb"
                      installDest: "hdb"
                    - installSource: "bin/hdb"
                      installDest: "bin/hdb"
                  exeSymLinked:
                    - linkName: "hdb-\${PKGVER}"
                      pVPMajorLinks: false
                      setName: "hdb"
                      target: "hdb"
                  dataRules:
                    - installPattern: ["lib/**"]
                  preserveMtimes: false
EOF

		# Use only our temporary channel (-s), install, and make it the default
		# so that the `hdb` symlink is created. Then run both symlinks that
		# ghcup creates: `hdb` and the versioned `hdb-<version>`.
		ghcup -s "file://${CHANNEL}" install haskell-debugger "${HDB_VERSION}" --set
		GHCUP_BIN="${GHCUP_INSTALL_BASE_PREFIX:-${HOME}}/.ghcup/bin"
		smoke_test "${GHCUP_BIN}/hdb"
		smoke_test "${GHCUP_BIN}/hdb-${HDB_VERSION}"
		;;

	*)
		echo "unknown mode '${MODE}', expected 'extract' or 'ghcup'" >&2
		exit 2
		;;
esac
