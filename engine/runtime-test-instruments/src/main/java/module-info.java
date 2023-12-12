module org.enso.runtime.test {
  requires org.graalvm.truffle;
  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.test.instruments.CodeIdsTestInstrumentProvider,
      org.enso.interpreter.test.instruments.CodeLocationsTestInstrumentProvider;
}
