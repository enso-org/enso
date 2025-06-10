module org.enso.ydoc.polyfill {
  requires io.helidon.webclient;
  requires io.helidon.webclient.websocket;
  requires io.helidon.webserver;
  requires io.helidon.webserver.websocket;
  requires org.enso.syntax;
  requires org.graalvm.polyglot;
  requires org.slf4j;

  opens org.enso.ydoc.polyfill.web;

  exports org.enso.ydoc.polyfill;
  exports org.enso.ydoc.polyfill.web;
}
