import org.enso.ydoc.server.registration.YdocServerImpl;

module org.enso.ydoc.server.registration {
  requires org.graalvm.polyglot;
  requires org.enso.jvm.interop;
  requires org.enso.jvm.channel;
  requires static org.graalvm.nativeimage;

  // only register service, otherwise the module isn't needed
  requires org.enso.runner.common;

  provides org.enso.runner.common.YdocServerApi with
      YdocServerImpl;
}
