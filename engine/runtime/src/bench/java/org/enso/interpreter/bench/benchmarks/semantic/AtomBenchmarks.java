package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.concurrent.TimeUnit;
import org.enso.interpreter.bench.fixtures.semantic.AtomFixtures;
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
public class AtomBenchmarks {
  private static AtomFixtures fixtures = new AtomFixtures();

  @Benchmark
  public void benchGenerateList() {
    DefaultInterpreterRunner.MainMethod main = fixtures.generateList();
    main.mainFunction().value().execute(fixtures.million());
  }

  @Benchmark
  public void benchGenerateListQualified() {
    DefaultInterpreterRunner.MainMethod main = fixtures.generateListQualified();
    main.mainFunction().value().execute(fixtures.million());
  }

  private void benchOnList(DefaultInterpreterRunner.MainMethod main) {
    main.mainFunction().value().execute(fixtures.millionElementList());
  }

  @Benchmark
  public void benchReverseList() {
    benchOnList(fixtures.reverseList());
  }

  @Benchmark
  public void benchReverseListMethods() {
    benchOnList(fixtures.reverseListMethods());
  }

  @Benchmark
  public void benchSumList() {
    benchOnList(fixtures.sumList());
  }

  @Benchmark
  public void sumListLeftFold() {
    benchOnList(fixtures.sumListLeftFold());
  }

  @Benchmark
  public void benchSumListFallback() {
    benchOnList(fixtures.sumListFallback());
  }

  @Benchmark
  public void benchSumListMethods() {
    benchOnList(fixtures.sumListMethods());
  }

  @Benchmark
  public void benchMapReverseList() {
    benchOnList(fixtures.mapReverseList());
  }

  @Benchmark
  public void benchMapReverseCurryList() {
    benchOnList(fixtures.mapReverseListCurry());
  }
}
