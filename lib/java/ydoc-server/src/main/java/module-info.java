module org.enso.ydoc.server {
  requires io.helidon.common;
  requires org.enso.ydoc.polyfill;
  requires org.graalvm.polyglot;
  requires org.slf4j;
  requires static org.graalvm.nativeimage;
  requires static org.enso.jvm.interop;

  // need for hosted JVM access
  opens org.enso.ydoc.server;
}
