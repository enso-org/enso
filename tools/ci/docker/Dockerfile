FROM oracle/graalvm-ce:20.2.0-java11

USER root

RUN groupadd -g 1001 enso
RUN useradd --system --create-home --uid 1001 --gid 1001 enso

ADD runner.jar /opt/enso/runner.jar
ADD runtime.jar /opt/enso/runtime.jar

RUN chown -hR enso:enso /opt/enso
RUN chmod -R u=rX,g=rX /opt/enso

RUN mkdir -p /home/enso/workspace
RUN chown -hR enso:enso /home/enso/workspace
RUN chmod -R u=rwX,g=rwX /home/enso/workspace

USER enso:enso

WORKDIR /opt/enso

ENTRYPOINT ["java", "-jar",  "-Dtruffle.class.path.append=runtime.jar", "-Dpolyglot.engine.IterativePartialEscape=true", "runner.jar" ]

EXPOSE 30001
EXPOSE 30002

VOLUME /home/enso/workspace

CMD ["--server", "--rpc-port", "30001", "--data-port", "30002", "--root-id", "00000000-0000-0000-0000-000000000001", "--path", "/home/enso/workspace", "--interface", "0.0.0.0"]

