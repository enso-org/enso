open module org.enso.interpreter.arrow {
  requires org.graalvm.truffle;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.arrow.ArrowLanguageProvider;
}
