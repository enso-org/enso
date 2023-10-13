module org.enso.runtime {
  requires java.base;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.EnsoLanguageProvider;
}
