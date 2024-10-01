module org.enso.runtime.instrument.id.execution {
  requires org.enso.runtime;
  requires org.enso.polyglot.api;
  requires org.graalvm.truffle;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.instrument.id.execution.IdExecutionInstrumentProvider;
}
