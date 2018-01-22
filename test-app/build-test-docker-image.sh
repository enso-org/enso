#!/bin/bash

set -eo pipefail;

docker build . -f Dockerfile-test -t amagdy/luna:test