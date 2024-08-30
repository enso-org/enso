module org.enso.librarymanager.test {
  requires scala.library;

  requires org.enso.cli;
  requires org.enso.distribution;
  requires org.enso.downloader;
  requires org.enso.librarymanager;
  requires org.enso.pkg;
  requires org.enso.semver;
  requires org.enso.editions;
  requires org.enso.process.utils;
  requires org.enso.version.output;
  // Automatic module
  requires org.enso.testkit;

  exports org.enso.librarymanager.test.published.repository;
}
