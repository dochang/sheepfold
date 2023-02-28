#!/bin/sh

set -e

: "${script_dir:?}"
root_dir="$(dirname "$script_dir")"
GUIX_PROFILE="$("$script_dir"/profile-name.sh)"
export GUIX_PROFILE
SHEEPFOLD_TEST_PROFILE="$GUIX_PROFILE"
export SHEEPFOLD_TEST_PROFILE
src_dir="$root_dir/src"
SHEEPFOLD_GUIX_OPTS="--load-path=$src_dir"
export SHEEPFOLD_GUIX_OPTS

cd "$root_dir"
