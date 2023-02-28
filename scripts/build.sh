#!/bin/sh

set -e

script_dir="$(dirname "$0")"
# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/common.sh"

: "${SHEEPFOLD_GUIX_BUILD_OPTS:=}"
export SHEEPFOLD_GUIX_BUILD_OPTS
guix_build_cmd="guix build $SHEEPFOLD_GUIX_OPTS $SHEEPFOLD_GUIX_BUILD_OPTS"

echo "Build packages..." >&2

# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/bootstrap-profile.sh"

# shellcheck disable=SC2086
"$script_dir/pkgnames.sh" "$@" | xargs --no-run-if-empty $guix_build_cmd
