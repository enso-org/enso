module org.enso.distribution {
  requires scala.library;
  requires org.enso.cli;
  requires org.enso.logging.utils;
  requires org.enso.scalalibs.wrapper;

  exports org.enso.distribution;
  exports org.enso.distribution.config;
  exports org.enso.distribution.locking;
}
