#!/bin/sh

set -e

if [ -n "$SHEEPFOLD_TEST_PROFILE" ]; then
    echo "$SHEEPFOLD_TEST_PROFILE"
elif [ -n "$SHEEPFOLD_TEST_PROFILE_PREFIX" ]; then
    echo "$SHEEPFOLD_TEST_PROFILE_PREFIX/profile"
else
    echo "$(mktemp --directory)/profile"
fi
