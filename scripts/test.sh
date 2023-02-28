#!/bin/sh

set -e

script_dir="$(dirname "$0")"
# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/common.sh"

guix_inst_cmd="guix package $SHEEPFOLD_GUIX_OPTS --profile=$GUIX_PROFILE --install"

echo "Test packages in profile $GUIX_PROFILE" >&2

# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/bootstrap-profile.sh"

# shellcheck disable=SC2086
"$script_dir/pkgnames.sh" "$@" | xargs --no-run-if-empty $guix_inst_cmd
