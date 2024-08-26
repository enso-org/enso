module org.enso.runtime.instrument.runtime.server {
  requires org.enso.runtime.instrument.common;
  requires org.enso.runtime;
  requires org.enso.polyglot.api;
  requires org.enso.engine.common;
  requires org.enso.distribution;
  requires org.enso.lockmanager;
  requires org.openide.util.lookup.RELEASE180;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.instrument.runtime.server.RuntimeServerInstrumentProvider;
}
