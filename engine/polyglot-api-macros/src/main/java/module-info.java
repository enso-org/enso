module org.enso.polyglot.api.macros {
  requires scala.library;
  // For jsoniter_scala
  requires org.enso.scala.wrapper;

  exports org.enso.polyglot.macros;
}
