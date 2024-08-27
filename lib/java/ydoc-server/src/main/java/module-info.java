module org.enso.ydoc {
  requires java.base;
  requires java.net.http;
  requires io.helidon.webserver;
  requires io.helidon.webserver.websocket;
  requires org.enso.profiling;
  requires org.enso.syntax;
  requires org.graalvm.polyglot;
  requires org.slf4j;

  opens org.enso.ydoc.polyfill.web;

  exports org.enso.ydoc;
}
