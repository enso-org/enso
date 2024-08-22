module org.enso.runtime.instrument.repl.debugger {
  requires scala.library;
  requires org.enso.runtime.instrument.common;
  requires org.enso.runtime;
  requires org.enso.polyglot.api;
  requires org.enso.runtime.compiler;
  requires org.graalvm.truffle;

  provides com.oracle.truffle.api.instrumentation.provider.TruffleInstrumentProvider with
      org.enso.interpreter.instrument.repl.debugger.ReplDebuggerInstrumentProvider;
}
