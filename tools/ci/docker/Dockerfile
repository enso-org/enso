FROM oracle/graalvm-ce:20.2.0-java11

USER root

ENV LOG_LEVEL=INFO

RUN groupadd -g 1001 enso
RUN useradd --system --create-home --uid 1001 --gid 1001 enso

ADD bin /opt/enso/bin
ADD component /opt/enso/component
ADD std-lib /opt/enso/std-lib

RUN chown -hR enso:enso /opt/enso
RUN chmod -R u=rX,g=rX /opt/enso
RUN chmod a+x /opt/enso/bin/*

RUN mkdir -p /volumes
RUN chown -hR enso:enso /volumes
RUN chmod -R u=rwX,g=rwX /volumes

USER enso:enso

WORKDIR /opt/enso

ENTRYPOINT [ "/opt/enso/bin/docker-entrypoint.sh" ]

EXPOSE 30001
EXPOSE 30002

CMD ["--server", "--daemon", "--rpc-port", "30001", "--data-port", "30002", "--root-id", "00000000-0000-0000-0000-000000000001", "--path", "/volumes/workspace", "--interface", "0.0.0.0"]
