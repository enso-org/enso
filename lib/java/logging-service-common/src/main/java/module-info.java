module org.enso.logging.service.common {
  requires java.net.http;
  requires scala.library;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires static org.enso.logging.service.logback;
  // For jsoniter_scala
  requires org.enso.scala.wrapper;
  requires org.slf4j;

  exports org.enso.logging.service.common;
}
