module org.enso.logging.service.telemetry {
  requires static org.enso.logging.service.logback;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires java.net.http;
  requires org.slf4j;

  provides org.enso.logging.service.logback.TelemetryAppender with
    org.enso.logging.service.telemetry.TelemetryAppenderImpl;
}
