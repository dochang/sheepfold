#!/bin/sh

set -e

root_dir="$(dirname "$(dirname "$0")")"
pkgs_dir="$root_dir/src/sheepfold/packages"

if [ "$#" -gt 0 ]; then
    for pkgname; do
        echo "$pkgname"
    done
elif [ -d "$pkgs_dir" ]; then
    find "$pkgs_dir" -type f -execdir sed -n 's/^.*(name "\(.*\)").*$/\1/p' {} \;
fi
