module org.enso.runtime.instrument.common {
  requires java.logging;
  requires scala.library;
  requires org.slf4j;

  requires org.enso.cli;
  requires org.enso.distribution;
  requires org.enso.lockmanager;
  requires org.enso.logging.utils;
  requires org.enso.editions;
  requires org.enso.engine.common;
  requires org.enso.refactoring;
  requires org.enso.runtime;
  requires org.enso.runtime.compiler;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.suggestions;
  requires org.enso.text.buffer;
  requires org.enso.pkg;
  requires org.enso.polyglot.api;
  // For com.typesafe.scalalogging
  requires org.enso.scala.wrapper;

  requires org.graalvm.truffle;
  requires org.graalvm.polyglot;
  // TODO: com.oracle.truffle.api.TruffleContext?

  exports org.enso.interpreter.instrument;
  exports org.enso.interpreter.service;
}
