module org.enso.runtime.version.manager {
  requires scala.library;
  requires org.apache.commons.compress;
  requires org.slf4j;
  requires org.enso.cli;
  requires org.enso.distribution;
  requires org.enso.downloader;
  requires org.enso.editions;
  requires org.enso.editions.updater;
  requires org.enso.logging.utils;
  requires org.enso.pkg;
  requires org.enso.semver;
  requires org.enso.scala.yaml;
  // For com.typesafe.scalalogging.Logger
  requires org.enso.scala.wrapper;
  requires org.enso.version.output;

  exports org.enso.runtimeversionmanager;
  exports org.enso.runtimeversionmanager.cli;
  exports org.enso.runtimeversionmanager.components;
  exports org.enso.runtimeversionmanager.runner;
}
