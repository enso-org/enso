package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.interpreter.bench.fixtures.semantic.RecursionFixtures;
import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(2)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class RecursionBenchmarks {
  private static RecursionFixtures recursionFixtures = new RecursionFixtures();

  @Benchmark
  public void benchSumTCO() {
    recursionFixtures.sumTCO().execute(recursionFixtures.hundredMillion());
  }

  @Benchmark
  public void benchSumTCOFoldLike() {
    recursionFixtures.sumTCOFoldLike().execute(recursionFixtures.hundredMillion());
  }

  @Benchmark
  public void benchSumRecursive() {
    recursionFixtures.sumTCO().execute(recursionFixtures.hundred());
  }

  @Benchmark
  public void benchOversaturatedRecursiveCall() {
    recursionFixtures.oversaturatedRecursiveCall().execute(recursionFixtures.hundredMillion());
  }
}
