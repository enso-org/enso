package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.concurrent.TimeUnit;
import org.enso.common.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 2, time = 5)
@Measurement(iterations = 3, time = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class FibGuestJavaPolyglotBenchmarks extends FibBenchmarks {

  @Override
  protected Context.Builder withModifiedContext(Context.Builder b) {
    return b.option(RuntimeOptions.HOST_CLASS_LOADING, RuntimeOptions.HOST_CLASS_LOADING_GUEST);
  }
}
