module org.enso.refactoring {
  requires scala.library;

  requires org.enso.text.buffer;
  requires org.enso.runtime.parser;

  exports org.enso.refactoring;
  exports org.enso.refactoring.validation;
}
