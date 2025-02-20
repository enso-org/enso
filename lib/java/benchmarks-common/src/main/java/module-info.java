module org.enso.benchmarks.common {
  requires org.enso.engine.common;
  requires org.enso.version.output;
  requires org.graalvm.polyglot;
  requires com.fasterxml.jackson.databind;
  requires com.networknt.schema;
  requires jmh.core;

  exports org.enso.interpreter.bench;
  opens org.enso.interpreter.bench.result to com.fasterxml.jackson.databind;
}
