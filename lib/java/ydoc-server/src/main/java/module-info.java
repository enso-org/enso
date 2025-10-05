import org.enso.ydoc.server.YdocServerImpl;

module org.enso.ydoc.server {
  requires io.helidon.common;
  requires org.enso.runner.common;
  requires org.enso.ydoc.polyfill;
  requires org.graalvm.polyglot;
  requires org.slf4j;
  requires org.enso.jvm.interop;
  requires static org.graalvm.nativeimage;
  requires static org.openide.util.lookup.RELEASE180;

  opens org.enso.ydoc.server;

  provides org.enso.runner.common.YdocServerApi with
      YdocServerImpl;
}
