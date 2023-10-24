module org.enso.runtime {
  requires java.base;
  requires java.compiler;
  requires java.desktop;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  requires org.slf4j;

  uses org.slf4j.spi.SLF4JServiceProvider;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.EnsoLanguageProvider,
      org.enso.interpreter.epb.EpbLanguageProvider;
  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
    org.enso.interpreter.instrument.ReplDebuggerInstrumentProvider;
}
