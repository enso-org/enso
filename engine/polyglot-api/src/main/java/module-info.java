module org.enso.polyglot.api {
  requires scala.library;
  requires flatbuffers.java;
  requires org.graalvm.truffle;
  requires org.graalvm.polyglot;

  requires org.enso.engine.common;
  requires org.enso.editions;
  requires org.enso.logging.utils;
  requires org.enso.scala.wrapper;
  requires org.enso.pkg;
  requires org.enso.polyglot.api.macros;
  requires org.enso.text.buffer;

  exports org.enso.polyglot;
  exports org.enso.polyglot.debugger;
  exports org.enso.polyglot.data;
  exports org.enso.polyglot.runtime;
  exports org.enso.polyglot.runtime.serde;
}
