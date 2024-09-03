module org.enso.librarymanager {
  requires scala.library;
  requires org.enso.distribution;
  requires org.enso.downloader;
  requires org.enso.cli;
  requires org.enso.editions;
  requires org.enso.pkg;
  requires org.enso.semver;
  requires org.enso.logging.utils;
  // For com.typesafe.scalalogging.Logger
  requires org.enso.scala.wrapper;
  requires org.enso.scala.yaml;

  requires org.yaml.snakeyaml;
  requires org.slf4j;

  exports org.enso.librarymanager;
  exports org.enso.librarymanager.dependencies;
  exports org.enso.librarymanager.local;
  exports org.enso.librarymanager.published;
  exports org.enso.librarymanager.published.bundles;
  exports org.enso.librarymanager.published.repository;
  exports org.enso.librarymanager.resolved;
  exports org.enso.libraryupload;
  exports org.enso.libraryupload.auth;
}
