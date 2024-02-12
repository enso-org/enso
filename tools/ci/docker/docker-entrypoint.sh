#!/bin/bash
set -e

if [ "$PRINT_VERSION" == "1" ]; then
  /opt/enso/bin/enso --version
fi

if [ "$LS_ROOT_ID" == "" ]; then
  LS_ROOT_ID="00000000-0000-0000-0000-000000000001"
fi

if [ "$INTERFACE" == "" ]; then
  INTERFACE="0.0.0.0"
fi

/opt/enso/bin/enso --log-level "$LOG_LEVEL" --rpc-port $RPC_PORT --data-port $DATA_PORT --root-id "$LS_ROOT_ID" --interface "$INTERFACE" "$@"
