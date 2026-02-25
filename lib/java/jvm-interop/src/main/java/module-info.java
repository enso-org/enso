module org.enso.jvm.interop {
  requires org.graalvm.polyglot;
  requires org.enso.persistance;
  requires org.graalvm.truffle;
  requires org.enso.jvm.channel;
  requires org.enso.engine.common;
  requires org.enso.logging.system2slf4j;
  requires org.slf4j;

  exports org.enso.jvm.interop.api;

  opens org.enso.jvm.interop.impl to
      org.enso.jvm.channel,
      org.slf4j;

  provides org.slf4j.spi.SLF4JServiceProvider with
      org.enso.jvm.interop.impl.OtherJvmLogger;
}
