#!/bin/bash

set -e #o pipefail;

tag=$(<.base-docker-image-tag)

docker push "$tag"