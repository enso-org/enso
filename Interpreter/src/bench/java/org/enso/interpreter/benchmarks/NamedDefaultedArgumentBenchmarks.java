package org.enso.interpreter.benchmarks;

import org.enso.interpreter.fixtures.NamedDefaultedArgumentFixtures;
import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(2)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class NamedDefaultedArgumentBenchmarks {
  private static NamedDefaultedArgumentFixtures argumentFixtures =
      new NamedDefaultedArgumentFixtures();

  @Benchmark
  public void benchSumTCOWithNamedArgs() {
    argumentFixtures.sumTCOWithNamedArguments().execute(argumentFixtures.hundredMillion());
  }

  @Benchmark
  public void benchSumTCOWithDefaultArgs() {
    argumentFixtures.sumTCOWithDefaultedArguments().execute(argumentFixtures.hundredMillion());
  }
}
