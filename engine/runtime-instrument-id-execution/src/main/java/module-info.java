module org.enso.runtime.instrument.id.execution {
  requires org.enso.runtime;
  requires org.enso.polyglot.api;
  requires org.graalvm.truffle;
  requires org.slf4j;
  requires org.enso.logging.utils;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.instrument.id.execution.IdExecutionInstrumentProvider;
}
