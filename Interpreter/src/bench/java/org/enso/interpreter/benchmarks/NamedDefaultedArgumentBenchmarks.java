package org.enso.interpreter.benchmarks;

import java.util.concurrent.TimeUnit;
import org.enso.interpreter.fixtures.NamedDefaultedArgumentFixtures;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Warmup;

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
