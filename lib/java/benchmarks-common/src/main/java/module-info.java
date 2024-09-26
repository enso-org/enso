module org.enso.benchmarks.common {
  requires jakarta.xml.bind;
  requires jmh.core;
  requires org.enso.engine.common;
  requires org.graalvm.polyglot;

  exports org.enso.interpreter.bench;

  opens org.enso.interpreter.bench to
      jakarta.xml.bind;
}
