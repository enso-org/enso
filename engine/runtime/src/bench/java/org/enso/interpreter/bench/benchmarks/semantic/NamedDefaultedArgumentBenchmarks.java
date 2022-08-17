package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.concurrent.TimeUnit;
import org.enso.interpreter.bench.fixtures.semantic.NamedDefaultedArgumentFixtures;
import org.enso.interpreter.test.DefaultInterpreterRunner;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class NamedDefaultedArgumentBenchmarks {
  private static NamedDefaultedArgumentFixtures argumentFixtures =
      new NamedDefaultedArgumentFixtures();

  private void runOnHundredMillion(DefaultInterpreterRunner.MainMethod main) {
    main.mainFunction().value().execute(argumentFixtures.hundredMillion());
  }

  @Benchmark
  public void benchSumTCOWithNamedArgs() {
    runOnHundredMillion(argumentFixtures.sumTCOWithNamedArguments());
  }

  @Benchmark
  public void benchSumTCOWithDefaultArgs() {
    runOnHundredMillion(argumentFixtures.sumTCOWithDefaultedArguments());
  }
}
