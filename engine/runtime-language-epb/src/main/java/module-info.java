open module org.enso.runtime.language.epb {
  requires java.logging;
  requires org.graalvm.truffle;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.epb.EpbLanguageProvider;
}
