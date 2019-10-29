package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.interpreter.bench.fixtures.semantic.AtomFixtures;
import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
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
  public void benchReverseListMethods() {
    fixtures.reverseListMethods().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchSumList() {
    fixtures.sumList().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void sumListLeftFold() {
    fixtures.sumListLeftFold().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchSumListFallback() {
    fixtures.sumListFallback().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchSumListMethods() {
    fixtures.sumListMethods().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchMapReverseList() {
    fixtures.mapReverseList().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchMapReverseCurryList() {
    fixtures.mapReverseListCurry().execute(fixtures.millionElementList());
  }
}
