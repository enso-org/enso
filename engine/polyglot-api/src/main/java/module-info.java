module org.enso.polyglot.api {
  requires flatbuffers.java;
  requires org.graalvm.truffle;

  exports org.enso.polyglot;
  exports org.enso.polyglot.debugger;
}
