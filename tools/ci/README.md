# CI Tools

This folder contains miscellaneous utilities for CI.

# Docker

## Building

A custom docker image requires a certain number of directories to be present
from a desired _edition_. The root directory of the docker build context can be
provided in the `docker build` command:

```bash
docker build -t <my-custom-name> -f tools/ci/docker/DockerFile --build-context docker-tools=tools/ci/docker built-distribution/enso-engine-$VERSION-linux-amd64/enso-$VERSION
```

where for a locally built distribution on Linux it would be `VERSION=0.0.0-dev`.

## Running

To start Language Server with a default configuration simply run the built image
with the chosen name:

```bash
docker run -t <my-custom-name>
```
