module org.enso.runtime.suggestions {
  requires scala.library;

  requires org.enso.pkg;
  requires org.enso.polyglot.api;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.compiler;
  requires org.enso.text.buffer;

  exports org.enso.compiler.suggestions;
}
