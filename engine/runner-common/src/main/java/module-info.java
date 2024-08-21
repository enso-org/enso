module org.enso.runner.common {
  requires commons.cli;
  requires org.apache.commons.io;
  requires org.enso.editions;
  requires org.enso.engine.common;
  requires org.enso.librarymanager;
  requires org.enso.pkg;
  requires org.slf4j;
  requires scala.library;

  exports org.enso.runner.common;
}
