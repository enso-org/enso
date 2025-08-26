module org.enso.logging.service.opensearch {
  requires java.net.http;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires org.enso.logging.service.logback;
  requires org.enso.logging.service;
  requires org.enso.logging.service.common;
  requires org.slf4j;

  exports org.enso.logging.service.opensearch;

  provides org.enso.logging.service.logback.AbstractRemoteAppender with
      org.enso.logging.service.opensearch.OpenSearchAppender;
}
