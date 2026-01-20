module org.enso.jvm.interop {
  requires org.graalvm.polyglot;
  requires org.enso.persistance;
  requires org.graalvm.truffle;
  requires org.enso.jvm.channel;
  requires org.enso.engine.common;

  exports org.enso.jvm.interop.api;

  opens org.enso.jvm.interop.impl to
      org.enso.jvm.channel;
}
