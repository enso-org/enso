module org.enso.lockmanager.server {
  requires scala.library;
  requires org.enso.scala.wrapper;
  requires org.enso.akka.wrapper;
  requires org.enso.distribution;
  requires org.enso.polyglot.api;

  exports org.enso.lockmanager.server;
}
