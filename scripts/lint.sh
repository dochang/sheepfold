#!/bin/sh

set -e

script_dir="$(dirname "$0")"
# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/common.sh"

guix_lint_cmd="guix lint $SHEEPFOLD_GUIX_OPTS --exclude=refresh,archival"
# Exclude `refresh` because it cannot find update for some git upstreams.
#
# Exclude `archival` because it displays annoying message for some packages.

echo "Lint packages in profile $GUIX_PROFILE" >&2

# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/bootstrap-profile.sh"

# shellcheck disable=SC2086
lint_msg="$("$script_dir/pkgnames.sh" "$@" | xargs --no-run-if-empty $guix_lint_cmd 2>&1)"

if echo "$lint_msg" | grep -v '^$' | grep -vq "fetching CVE database for"; then
    echo "$lint_msg"
    exit 1
fi
