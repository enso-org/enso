FROM ghcr.io/graalvm/jdk-community:21

USER root

ARG LOG_LEVEL=info
ARG RPC_PORT=30001
ARG DATA_PORT=30002
ARG YDOC_PORT=1234
ARG PRINT_VERSION=0
ARG JAVA_OPTS="-XX:MaxRAMPercentage=90.0 -XX:InitialRAMPercentage=90.0"
ARG PROFILING_FILENAME
ARG PROFILING_TIME

RUN useradd -u 2000 -c 'Enso Developer' -U -m ensodev

# /opt/enso is the present engine distribution.
# /opt/workdir is a directory for temporary runtime files and logs.
# /volumes/workspace is the root of the mounted workspace which contains all data that must persist when the project is reopened.
# /volumes/workspace/project_root contains the project package.
# /volumes/workspace/data_root contains the data root of the Enso distribution, this is where cached libraries will be located.
# /volumes/workspace/config contains configuration files, currently these are not really used in the Cloud.
# /volumes/workspace/home contains the ENSO_HOME directory, where locally created libraries will be placed.
# Currently, only the /volumes/workspace/project_root needs to be initialized with the project structure when the project is created.
# All other directories are created on-demand.

ENV ENSO_DATA_DIRECTORY=/volumes/workspace/data_root
ENV ENSO_CONFIG_DIRECTORY=/volumes/workspace/config
ENV ENSO_RUNTIME_DIRECTORY=/opt/enso/work
ENV ENSO_LOG_DIRECTORY=/opt/enso/logs
ENV ENSO_HOME=/volumes/workspace/home

COPY --chown=ensodev:ensodev --chmod=555 bin /opt/enso/bin
COPY --chown=ensodev:ensodev --chmod=555 --from=docker-tools docker-entrypoint.sh /opt/enso/bin/
COPY --chown=ensodev:ensodev --chmod=554 component /opt/enso/component
COPY --chown=ensodev:ensodev --chmod=554 lib /opt/enso/lib
COPY --chown=ensodev:ensodev --chmod=554 editions /opt/enso/editions

# When the non-octal COPY --chmod support `moby/buildkit#5380` is stabilized,
# it will be possible to use the following syntax. It allows to set more precise
# chmod modifiers, i.e. the `X` attribute here does not mark files executable
# while keeping the executable flag for directories.
#COPY --chown=ensodev:ensodev --chmod=a=rx bin /opt/enso/bin
#COPY --chown=ensodev:ensodev --chmod=a=rx --from=docker-tools docker-entrypoint.sh /opt/enso/bin/
#COPY --chown=ensodev:ensodev --chmod=u=rX,g=rX component /opt/enso/component
#COPY --chown=ensodev:ensodev --chmod=u=rX,g=rX lib /opt/enso/lib
#COPY --chown=ensodev:ensodev --chmod=u=rX,g=rX editions /opt/enso/editions

RUN mkdir -p /volumes
RUN chown -hR ensodev:ensodev /volumes
RUN chmod -R u=rwX,g=rwX /volumes

USER ensodev:ensodev

RUN mkdir -m 777 /opt/enso/work
RUN mkdir -m 777 /opt/enso/logs
RUN mkdir -m 777 /opt/enso/profiling

WORKDIR /opt/enso

ENTRYPOINT [ "/opt/enso/bin/docker-entrypoint.sh" ]

ENV RPC_PORT=${RPC_PORT}
ENV DATA_PORT=${DATA_PORT}
ENV YDOC_PORT=${YDOC_PORT}
ENV LOG_LEVEL=${LOG_LEVEL}
ENV PRINT_VERSION=${PRINT_VERSION}
ENV JAVA_OPTS=${JAVA_OPTS}

EXPOSE ${RPC_PORT}
EXPOSE ${DATA_PORT}
EXPOSE ${YDOC_PORT}


CMD ["--server", "--daemon", "--path", "/volumes/workspace/project_root"]
