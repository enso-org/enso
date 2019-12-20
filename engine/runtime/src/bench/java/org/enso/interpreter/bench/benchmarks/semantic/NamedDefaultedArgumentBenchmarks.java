package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.interpreter.bench.fixtures.semantic.NamedDefaultedArgumentFixtures;
import org.enso.interpreter.test.InterpreterRunner;
import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class NamedDefaultedArgumentBenchmarks {
  private static NamedDefaultedArgumentFixtures argumentFixtures =
      new NamedDefaultedArgumentFixtures();

  private void runOnHundredMillion(InterpreterRunner.MainMethod main) {
    main.mainFunction().execute(main.mainConstructor(), argumentFixtures.hundredMillion());
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
