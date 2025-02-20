module org.enso.bench.processor {
  requires java.compiler;
  requires java.logging;
  requires org.enso.engine.common;
  requires org.enso.polyglot.api;
  requires org.enso.runtime;
  requires org.graalvm.polyglot;
  requires org.openide.util.lookup.RELEASE180;

  exports org.enso.benchmarks;
  exports org.enso.benchmarks.processor;

  provides javax.annotation.processing.Processor with
      org.enso.benchmarks.processor.BenchProcessor;
}
