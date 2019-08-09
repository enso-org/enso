package org.enso.interpreter.benchmarks;

import org.enso.interpreter.fixtures.AtomFixtures;
import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(2)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class AtomBenchmarks {
  private static AtomFixtures fixtures = new AtomFixtures();

  @Benchmark
  public void benchGenerateList() {
    fixtures.generateList().execute(fixtures.million());
  }

  @Benchmark
  public void benchReverseList() {
    fixtures.reverseList().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchSumList() {
    fixtures.sumList().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchSumListFallback() {
    fixtures.sumListFallback().execute(fixtures.millionElementList());
  }
}
