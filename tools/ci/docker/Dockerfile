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

ADD bin /opt/enso/bin
ADD component /opt/enso/component
ADD lib /opt/enso/lib
ADD editions /opt/enso/editions

RUN mkdir /opt/enso/work
RUN mkdir /opt/enso/logs
RUN mkdir /opt/enso/profiling

ENV ENSO_DATA_DIRECTORY=/volumes/workspace/data_root
ENV ENSO_CONFIG_DIRECTORY=/volumes/workspace/config
ENV ENSO_RUNTIME_DIRECTORY=/opt/enso/work
ENV ENSO_LOG_DIRECTORY=/opt/enso/logs
ENV ENSO_HOME=/volumes/workspace/home

RUN chown -hR ensodev:ensodev /opt/enso
RUN chmod -R u=rX,g=rX /opt/enso
RUN chmod a+x /opt/enso/bin/*
RUN chmod a+rw /opt/enso/work
RUN chmod a+rw /opt/enso/logs
RUN chmod a+rw /opt/enso/profiling

RUN mkdir -p /volumes
RUN chown -hR ensodev:ensodev /volumes
RUN chmod -R u=rwX,g=rwX /volumes
COPY --from=docker-tools docker-entrypoint.sh /opt/enso/bin/

USER ensodev:ensodev

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
