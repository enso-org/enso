#!/usr/bin/env bash

# Copy built Enso distribution from Bazel output directory to the directory in the source tree.
# It allows write access to the distribution files which is necessary for signing on Mac.
set -euo pipefail

if [[ "$#" -ne 2 ]]; then
  echo "Usage: stage-bazel-app.sh <bazel-ide-dist-dir> <workspace-relative-output-dir>" >&2
  exit 1
fi

source_dir_input="$1"
output_dir_rel="$2"

initial_dir="$PWD"
workspace_dir="${BUILD_WORKSPACE_DIRECTORY:-$PWD}"

if [[ "$source_dir_input" = /* ]]; then
  source_dir="$source_dir_input"
else
  source_dir="$initial_dir/$source_dir_input"
fi

cd "$workspace_dir"

if [[ -d "$source_dir/mac-arm64/Enso.app" ]]; then
  app_arch="mac-arm64"
elif [[ -d "$source_dir/mac/Enso.app" ]]; then
  app_arch="mac"
else
  echo "Unable to find Enso.app under '$source_dir/mac-arm64' or '$source_dir/mac'." >&2
  exit 1
fi

source_app="$source_dir/$app_arch/Enso.app"
dest_dir="$workspace_dir/$output_dir_rel/$app_arch"
dest_app="$dest_dir/Enso.app"

mkdir -p "$dest_dir"
chmod -R u+w "$dest_app" >/dev/null 2>&1 || true
rm -rf "$dest_app"
ditto "$source_app" "$dest_app"
chmod -R u+w "$dest_app"

echo "Staged app: $dest_app"
