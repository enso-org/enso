module org.enso.runner {
  requires commons.cli;
  requires org.enso.cli;
  requires org.enso.distribution;
  requires org.enso.editions;
  requires org.enso.editions.updater;
  requires org.enso.engine.common;
  requires org.enso.librarymanager;
  requires org.enso.logging.config;
  requires org.enso.logging.utils;
  requires org.enso.runtime.parser;
  requires org.enso.runner.common;
  requires org.enso.pkg;
  requires org.enso.polyglot.api;
  requires org.enso.profiling;
  requires org.enso.semver;
  requires org.enso.version.output;
  requires org.graalvm.nativeimage;
  requires org.graalvm.polyglot;
  requires org.jline;
  requires scala.library;
  requires org.slf4j;
}
