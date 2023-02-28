#!/bin/sh

set -e

script_dir="$(dirname "$0")"
GUIX_PROFILE="$("$script_dir"/profile-name.sh)"
export GUIX_PROFILE

guix package --profile="$GUIX_PROFILE" --install findutils
# Ensure `xargs` supports `--no-run-if-empty` which is a GNU extension.

# https://www.shellcheck.net/wiki/SC1091
# shellcheck source=/dev/null
. "$GUIX_PROFILE/etc/profile"
