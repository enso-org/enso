open module org.enso.runtime {
  requires java.base;
  requires java.compiler;
  requires java.desktop;
  requires java.se;
  // Because of akka.util.Unsafe
  requires jdk.unsupported;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  requires static org.slf4j;
  requires org.bouncycastle.provider;

  uses org.slf4j.spi.SLF4JServiceProvider;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.EnsoLanguageProvider,
      org.enso.interpreter.epb.EpbLanguageProvider;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
    org.enso.interpreter.instrument.ReplDebuggerInstrumentProvider,
    org.enso.interpreter.instrument.RuntimeServerInstrumentProvider,
    org.enso.interpreter.instrument.IdExecutionInstrumentProvider;
}
