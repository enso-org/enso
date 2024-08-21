module org.enso.polyglot.api {
  requires flatbuffers.java;
  requires org.graalvm.truffle;
  requires org.graalvm.polyglot;
  requires org.enso.engine.common;
  requires org.enso.scala.wrapper;

  exports org.enso.polyglot;
  exports org.enso.polyglot.debugger;
}
