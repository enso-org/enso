module org.enso.runtime.test {
  requires org.graalvm.truffle;
  requires org.openide.util.lookup.RELEASE180;

  exports org.enso.interpreter.test.instruments;
  exports org.enso.interpreter.test.instruments.service;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.test.instruments.CodeIdsTestInstrumentProvider,
      org.enso.interpreter.test.instruments.CodeLocationsTestInstrumentProvider,
      org.enso.interpreter.test.instruments.NodeCountingTestInstrumentProvider;
}
