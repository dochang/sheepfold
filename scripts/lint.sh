#!/bin/sh

set -e

script_dir="$(dirname "$0")"
# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/common.sh"

guix_lint_cmd="guix lint $SHEEPFOLD_GUIX_OPTS --exclude=refresh,archival,input-labels"
# Exclude `refresh` because it cannot find update for some git upstreams.
#
# Exclude `archival` because it displays annoying message for some packages.
#
# Exclude `input-labels` because it doesn't support inputs with outputs.  See
# [1] & [2] for details.
#
# [1]: https://guix.gnu.org/en/manual/devel/en/html_node/package-Reference.html#index-inputs_002c-of-packages
# [2]: https://git.savannah.gnu.org/cgit/guix.git/tree/guix/lint.scm?id=b7e77446261fdc8dab360d7835a5dec919f6a79f#n546

echo "Lint packages in profile $GUIX_PROFILE" >&2

# https://www.shellcheck.net/wiki/SC1090
# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=scripts/bootstrap-profile.sh
. "$script_dir/bootstrap-profile.sh"

# shellcheck disable=SC2086
lint_msg="$("$script_dir/pkgnames.sh" "$@" | xargs --no-run-if-empty $guix_lint_cmd 2>&1)"
echo "$lint_msg" >&2

exclude_file="$script_dir/lint-message-exclude-patterns"
err_msg="$(echo "$lint_msg" | grep -v -f "$exclude_file" || true)"

if [ -n "$err_msg" ]; then
    exit 1
fi
