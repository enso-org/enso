FROM node:20.16-bookworm-slim

USER root

ARG LOG_LEVEL=info
ARG YDOC_SERVER_PORT=5976
ARG YDOC_SERVER_HOSTNAME=localhost
ARG YDOC_SERVER_LANGUAGE_SERVER_URL
ARG YDOC_SERVER_DEBUG=false

RUN useradd -u 2000 -c 'Enso Developer' -U -m ensodev

RUN mkdir /opt/ydoc-server-nodejs
RUN mkdir /opt/ydoc-server-nodejs/logs
RUN mkdir /opt/ydoc-server-nodejs/bin

ADD dist/* /opt/ydoc-server-nodejs

COPY --from=docker-tools ydoc-server-entrypoint.sh /opt/ydoc-server-nodejs/bin/

RUN chown -hR ensodev:ensodev /opt/ydoc-server-nodejs
RUN chmod -R u=rX,g=rX /opt/ydoc-server-nodejs
RUN chmod a+rw /opt/ydoc-server-nodejs/logs
RUN chmod a+rw /opt/ydoc-server-nodejs/logs
RUN chmod a+x /opt/ydoc-server-nodejs/bin/*

USER ensodev:ensodev

WORKDIR /opt/ydoc-server-nodejs

ENTRYPOINT [ "/opt/ydoc-server-nodejs/bin/ydoc-server-entrypoint.sh" ]

ENV LOG_LEVEL=${LOG_LEVEL}
ENV PORT=${YDOC_SERVER_PORT}
ENV HOSTNAME=${YDOC_SERVER_HOSTNAME}
ENV LANGUAGE_SERVER_URL=${YDOC_SERVER_LANGUAGE_SERVER_URL}
ENV ENSO_YDOC_LS_DEBUG=${YDOC_SERVER_DEBUG}

EXPOSE ${PORT}
