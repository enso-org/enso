#!/bin/bash

set -e #o pipefail;

tag=$(<.base-docker-image-tag)

docker build . -f Dockerfile -t "$tag"