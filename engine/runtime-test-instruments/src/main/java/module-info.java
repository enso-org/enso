module org.enso.runtime.test.instrument {
  requires org.graalvm.truffle;
  exports org.enso.interpreter.test.instrument;
  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.test.instrument.CodeLocationsTestInstrumentProvider,
      org.enso.interpreter.test.instrument.CodeIdsTestInstrumentProvider;
}
