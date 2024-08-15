module org.enso.runner {
  requires commons.cli;
  requires org.enso.cli;
  requires org.enso.distribution;
  requires org.enso.editions;
  requires org.enso.editions.updater;
  requires org.enso.librarymanager;
  requires org.enso.runtime.parser;
  requires org.enso.runner.common;
  requires org.enso.pkg;
  requires org.enso.profiling;
  requires org.enso.semver;
  requires org.graalvm.nativeimage;
  requires org.graalvm.polyglot;
  requires org.jline;
  requires org.slf4j;
  requires scala.library;
}
