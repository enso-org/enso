module org.enso.logging.service.telemetry {
  requires java.net.http;
  requires scala.library;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires static org.enso.logging.service.logback;
  // For jsoniter_scala
  requires org.enso.scala.wrapper;
  requires org.slf4j;
  requires static org.openide.util.lookup.RELEASE180;

  provides org.enso.logging.service.logback.TelemetryAppender with
    org.enso.logging.service.telemetry.TelemetryAppenderImpl;
}
