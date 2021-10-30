package org.enso;

import org.openjdk.jmh.annotations.*;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3)
@Measurement(iterations = 3)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class JavaBenchmarks {

  public long sumNumbers(long i) {
    long result = 0;
    while (i-- > 0) result += i;
    return result;
  }

  @Benchmark
  public void sumNumbersBenchmark() {
    long result = sumNumbers(100000000);
  }

  @State(Scope.Benchmark)
  public static class ListState {
    public Cons list = Cons.genList(1000000);
  }

  @Benchmark
  public void sumListBenchmark(ListState state) {
    state.list.sum();
  }

  @Benchmark
  public void reverseListBenchmark(ListState state) {
    state.list.reverse();
  }

  @Benchmark
  public void sumListFoldBenchmark(ListState state) {
    state.list.fold(((Long x, Long y) -> x + y), 0L);
  }
}
