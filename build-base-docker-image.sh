#!/bin/bash

set -eo pipefail;

docker build . -f Dockerfile -t amagdy/luna:base