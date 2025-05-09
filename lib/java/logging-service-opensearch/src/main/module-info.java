module org.enso.logging.service.opensearch {
  requires java.net.http;
  requires scala.library;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires static org.enso.logging.service.logback;
  requires org.enso.logging.service.remote;
  // For jsoniter_scala
  requires org.enso.scala.wrapper;
  requires org.slf4j;
  requires static org.openide.util.lookup.RELEASE180;

  provides org.enso.logging.service.logback.RemoteAppender with
          org.enso.logging.service.opensearch.OpenSearchAppender;
}
