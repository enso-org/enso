open module org.enso.runtime.language.epb {
  requires java.logging;
  requires org.graalvm.polyglot;
  requires org.graalvm.truffle;
  requires org.enso.runtime.utils;
  requires org.enso.ydoc.polyfill;
  requires org.enso.jvm.channel;
  requires org.enso.jvm.interop;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.epb.EpbLanguageProvider;
}
