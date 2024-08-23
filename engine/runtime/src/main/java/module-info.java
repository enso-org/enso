module org.enso.runtime {
  requires java.logging;
  requires scala.library;

  requires org.enso.cli;
  requires org.enso.common.polyglot.core.utils;
  requires org.enso.distribution;
  requires org.enso.editions;
  requires org.enso.engine.common;
  requires static org.enso.interpreter.dsl;
  requires static org.enso.persistance;
  requires org.enso.librarymanager;
  requires org.enso.lockmanager;
  requires org.enso.logging.utils;
  requires org.enso.pkg;
  requires org.enso.polyglot.api;
  requires org.enso.runtime.compiler;
  requires org.enso.runtime.parser;
  requires org.enso.runtime.suggestions;
  requires org.enso.text.buffer;
  requires org.enso.syntax;
  requires org.enso.version.output;

  requires org.apache.commons.lang3;
  requires org.apache.tika.core;
  requires org.slf4j;
  requires org.graalvm.collections;
  requires org.graalvm.truffle;
  requires com.ibm.icu;

  exports org.enso.interpreter.node;
  exports org.enso.interpreter.runtime.callable;
  exports org.enso.interpreter.runtime.state;
  exports org.enso.interpreter.runtime;
  exports org.enso.interpreter.runtime.data;
  exports org.enso.interpreter.runtime.data.atom;
  exports org.enso.interpreter.runtime.data.text;
  exports org.enso.interpreter.node.callable;
  exports org.enso.interpreter.runtime.control;
  exports org.enso.interpreter.runtime.callable.function;
  exports org.enso.interpreter.runtime.library.dispatch;
  exports org.enso.interpreter.runtime.error;
  exports org.enso.interpreter.runtime.instrument;
  exports org.enso.interpreter.runtime.tag;
  exports org.enso.interpreter.runtime.scope;
  exports org.enso.interpreter.runtime.type;
  exports org.enso.interpreter.runtime.warning;
  exports org.enso.interpreter.node.expression.builtin;
  exports org.enso.interpreter.node.expression.builtin.debug;
  exports org.enso.interpreter.node.expression.builtin.text.util;
  exports org.enso.interpreter.node.expression.debug;
  exports org.enso.interpreter.util;
  exports org.enso.interpreter.service.error;

  provides com.oracle.truffle.api.provider.TruffleLanguageProvider with
      org.enso.interpreter.EnsoLanguageProvider;
}
