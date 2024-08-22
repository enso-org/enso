module org.enso.runtime {
  requires java.logging;
  requires scala.library;

  requires org.enso.librarymanager;
  requires org.enso.logging.utils;
  requires org.enso.pkg;
  requires org.enso.polyglot.api;
  requires org.enso.runtime.parser;
  requires org.enso.syntax;
  requires org.enso.version.output;

  requires org.apache.commons.lang3;
  requires org.apache.tika.core;
  requires org.graalvm.collections;
  requires org.graalvm.truffle;
  requires org.openide.util.lookup.RELEASE180;
  requires org.slf4j;
  requires com.ibm.icu;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.EnsoLanguageProvider;
}
