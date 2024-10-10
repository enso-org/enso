module org.enso.ydoc {
  requires io.helidon.webclient;
  requires io.helidon.webclient.websocket;
  requires io.helidon.webserver;
  requires io.helidon.webserver.websocket;
  requires org.enso.syntax;
  requires org.graalvm.polyglot;
  requires org.slf4j;
  requires static org.graalvm.nativeimage;

  opens org.enso.ydoc.polyfill.web;

  exports org.enso.ydoc;
}
