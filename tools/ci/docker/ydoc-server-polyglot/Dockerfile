FROM jeanblanchard/alpine-glibc

USER root

ARG LOG_LEVEL=info
ARG YDOC_SERVER_PORT=5976
ARG YDOC_SERVER_HOSTNAME=localhost
ARG YDOC_SERVER_LANGUAGE_SERVER_URL
ARG YDOC_SERVER_DEBUG=false

RUN addgroup -S ensodev
RUN adduser -S -u 2000 -g 'Enso Developer' -G ensodev ensodev

RUN mkdir -p /opt/ydoc-server-polyglot/bin

RUN chown -hR ensodev:ensodev /opt/ydoc-server-polyglot
RUN chmod -R u=rX,g=rX /opt/ydoc-server-polyglot

COPY --from=native-image --chown=ensodev:ensodev --chmod=555 ydoc /opt/ydoc-server-polyglot/bin/

USER ensodev:ensodev

WORKDIR /opt/ydoc-server-polyglot

ENTRYPOINT [ "/opt/ydoc-server-polyglot/bin/ydoc" ]

ENV LOG_LEVEL=${LOG_LEVEL}
ENV PORT=${YDOC_SERVER_PORT}
ENV HOSTNAME=${YDOC_SERVER_HOSTNAME}
ENV LANGUAGE_SERVER_URL=${YDOC_SERVER_LANGUAGE_SERVER_URL}
ENV ENSO_YDOC_LS_DEBUG=${YDOC_SERVER_DEBUG}

EXPOSE ${PORT}
