# FROM ubuntu:16.04
FROM haskell:8.2.1

# # Install Haskell Stack https://docs.haskellstack.org/en/stable/README/
# RUN curl -sSL https://get.haskellstack.org/ | sh

RUN mkdir -p /src/luna

RUN apt-get update && \
    apt-get upgrade -y && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends apt-utils && \
    apt-get install -y --no-install-recommends xz-utils make && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Add $LUNA_REPO_PATH/dist/bin/public/luna to $PATH
ENV PATH="/src/luna/dist/bin/public/luna:${PATH}"
# Set LUNA_HOME to $LUNA_REPO_PATH/stdlib
ENV LUNA_HOME="/src/luna/stdlib"

WORKDIR /src
RUN git clone --depth=1 https://github.com/luna/luna.git luna
# COPY . /src/luna

WORKDIR /src/luna/shell

# --ghc-options="-O2 -j4"
RUN stack install --install-ghc
