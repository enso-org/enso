#!/usr/bin/env bash

set -eou pipefail

if [[ -n "${GITHUB_SHA:-}" ]]; then
  IDE_COMMIT_HASH="$GITHUB_SHA"
else
  IDE_COMMIT_HASH="$(git rev-parse --verify HEAD)"
fi

if [[ -n "${ENSO_VERSION:-}" ]]; then
  IDE_VERSION="$ENSO_VERSION"
else
  IDE_VERSION="$(date +%Y.%-m.%-d)-dev"
fi


echo "STABLE_IDE_VERSION $IDE_VERSION"
echo "STABLE_IDE_COMMIT_HASH $IDE_COMMIT_HASH"