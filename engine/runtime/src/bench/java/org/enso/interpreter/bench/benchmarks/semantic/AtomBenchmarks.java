package org.enso.interpreter.bench.benchmarks.semantic;

import org.enso.interpreter.bench.fixtures.semantic.AtomFixtures;
import org.enso.interpreter.test.InterpreterRunner;
import org.graalvm.polyglot.PolyglotException;
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
    InterpreterRunner.MainMethod main = fixtures.generateList();
    main.mainFunction().value().execute(main.mainConstructor(), fixtures.million());
  }

  private void benchOnList(InterpreterRunner.MainMethod main) {
    main.mainFunction().value().execute(main.mainConstructor(), fixtures.millionElementList());
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
