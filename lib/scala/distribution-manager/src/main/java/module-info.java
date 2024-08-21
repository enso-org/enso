module org.enso.distribution {
  requires scala.library;
  requires org.enso.cli;
  requires org.enso.logging.utils;
  requires org.enso.scala.wrapper;
  requires org.enso.scala.yaml;

  exports org.enso.distribution;
  exports org.enso.distribution.config;
  exports org.enso.distribution.locking;
}
