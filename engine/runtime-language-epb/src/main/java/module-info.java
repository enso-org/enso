open module org.enso.runtime.language.epb {
  requires java.logging;
  requires org.graalvm.truffle;
  requires org.enso.ydoc.polyfill;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.epb.EpbLanguageProvider;
}
