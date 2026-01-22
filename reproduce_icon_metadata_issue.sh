#!/usr/bin/env bash

set -eou pipefail

ICON_NAME_FILE=app/gui/src/project-view/util/iconMetadata/iconName.ts
TSCONFIG_FILE=app/gui/tsconfig.app.json

# Get to the clean state
jj restore $TSCONFIG_FILE
rm -rf $ICON_NAME_FILE
# Run write_all immediately
bazel run //:write_all > /dev/null 2>&1

# Observe that tsconfig is changing
if [ "$(jj diff $TSCONFIG_FILE | wc -l)" -gt 1 ]; then
    echo "tsconfig have changed(1)"
else
    echo "tsconfig haven’t changed(1)"
fi

# Remove the generated file
rm -rf $ICON_NAME_FILE

# Generate metadata, then run //:write_all
bazel run //app/gui:write_icon_metadata > /dev/null 2>&1 
bazel run //:write_all > /dev/null 2>&1

# Observe no changes to tsconfig
if [ "$(jj diff $TSCONFIG_FILE | wc -l)" -gt 1 ]; then
    echo "tsconfig have changed(2)"
else
    echo "tsconfig haven’t changed(2)"
fi


