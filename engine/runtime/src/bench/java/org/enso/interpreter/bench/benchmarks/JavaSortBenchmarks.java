package org.enso.interpreter.bench.benchmarks;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class JavaSortBenchmarks {
  public static int size = 1000000;

  public static Object[] makeSortedAscending(int size) {
    var array = new Object[size];
    for (int i = 0; i < size; ++i) {
      array[i] = (long) i;
    }
    return array;
  }

  public static Object[] makePartiallySorted(int size) {
    var generator = new Random(size);
    var array = new Object[size];
    var lastNumber = 0L;
    var runLength = 0L;
    var sortDirection = 1; // 1 for ascending, -1 for descending

    for (int i = 0; i < size; ++i) {
      if (runLength == 0) {
        sortDirection = (generator.nextDouble() > 0) ? 1 : -1;
        runLength = (generator.nextLong() % (size / 10)) - 1;
        lastNumber = generator.nextInt();
      } else {
        var change = Math.abs(generator.nextInt()) % size;
        if (sortDirection > 0) {
          lastNumber += change;
        } else {
          lastNumber -= change;
        }
        --runLength;
      }
      array[i] = lastNumber;
    }
    return array;
  }

  public static Object[] makeRandom(int size) {
    var generator = new Random(size);
    var array = new Object[size];
    for (int i = 0; i < size; ++i) {
      array[i] = generator.nextLong();
    }
    return array;
  }

  @State(Scope.Thread)
  public static class BenchState {
    public Object[] sorted;
    public Object[] partiallySorted;
    public Object[] random;

    @Setup(Level.Iteration)
    public void doSetup() {
      sorted = makeSortedAscending(size);
      partiallySorted = makePartiallySorted(size);
      random = makeRandom(size);
    }

    @TearDown(Level.Iteration)
    public void doTeardown() {
      sorted = null;
      partiallySorted = null;
      random = null;
    }
  }

  @Benchmark
  public void alreadySorted(BenchState state) {
    Arrays.sort(state.sorted);
  }

  @Benchmark
  public void sortedOppositeOrder(BenchState state) {
    Comparator<Object> comp = (Object l, Object r) -> ((Long) r).compareTo((Long) l);
    Arrays.sort(state.sorted, comp);
  }

  @Benchmark
  public void sortedRunsAscending(BenchState state) {
    Arrays.sort(state.partiallySorted);
  }

  @Benchmark
  public void sortedRunsDescending(BenchState state) {
    Comparator<Object> comp = (Object l, Object r) -> ((Long) r).compareTo((Long) l);
    Arrays.sort(state.partiallySorted, comp);
  }

  @Benchmark
  public void randomElementsAscending(BenchState state) {
    Comparator<Object> comp = (Object l, Object r) -> ((Long) l).compareTo((Long) r);
    Arrays.sort(state.random, comp);
  }

  @Benchmark
  public void randomElementsDescending(BenchState state) {
    Comparator<Object> comp = (Object l, Object r) -> ((Long) r).compareTo((Long) l);
    Arrays.sort(state.random, comp);
  }

  @Benchmark
  public void customProjection(BenchState state) {
    Comparator<Object> comp =
        (Object l, Object r) -> {
          Long new_l = (Long) l % 10;
          Long new_r = (Long) r % 10;
          return new_l.compareTo(new_r);
        };
    Arrays.sort(state.random, comp);
  }

  @Benchmark
  public void customComparison(BenchState state) {
    Comparator<Object> comp =
        (Object l, Object r) -> {
          return ((Long) r).compareTo((Long) l);
        };
    Arrays.sort(state.random, comp);
  }
}
