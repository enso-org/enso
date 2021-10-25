#!/bin/bash
set -e

echo "Starting Enso Runtime in version"

/opt/enso/bin/enso --version

/opt/enso/bin/enso --log-level $LOG_LEVEL "$@"
