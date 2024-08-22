module org.enso.lockmanager {
  requires scala.library;

  requires org.enso.distribution;
  requires org.enso.polyglot.api;
  requires org.enso.scala.wrapper;

  exports org.enso.lockmanager.client;
}
