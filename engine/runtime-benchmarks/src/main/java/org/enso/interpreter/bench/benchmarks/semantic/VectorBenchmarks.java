package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.AbstractList;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import org.enso.compiler.benchmarks.Utils;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class VectorBenchmarks {
  private Value arrayOfFibNumbers;
  private Value avg;
  private Value self;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Utils.createDefaultContextBuilder().build();
    var benchmarkName = SrcUtil.findName(params);
    var code =
        """
        from Standard.Base import all
        import Standard.Base.Data.Array_Proxy.Array_Proxy

        avg arr =
            sum acc i =
                stop = i == arr.length
                if stop then acc else
                    value = arr.at i
                    both = acc + value
                    @Tail_Call sum both i+1
            (sum 0 0) / arr.length

        fibarr size modulo =
            Vector.build initial_capacity=size propagate_warnings=False b->
                b.append 1
                b.append 1

                add_more n = if n == size then b else
                    b.append <| (b.at n-1 + b.at n-2) % modulo
                    @Tail_Call add_more n+1

                add_more 2

        to_vector arr = Vector.from_polyglot_array arr
        to_array vec = vec.to_array
        slice n vec start end =
            initial = vec.slice start end

            complicate round v = if round != 0 then complicate round-1 (v.slice 1 v.length) else
                Runtime.assert v==initial
                v

            if n==0 then initial else
                complicate n (0.up_to n).to_vector+initial

        fill_proxy proxy vec =
          size v =
            _ = v
            vec.length
          at i = vec.at i
          proxy.init size at
        create_array_proxy vec =
          Array_Proxy.from_proxy_object vec
        create_array_proxy_new vec =
          Array_Proxy.new vec.length (i -> vec.at i)
        """;

    var module = ctx.eval(SrcUtil.source(benchmarkName, code));

    this.self = module.invokeMember("get_associated_type");
    Function<String, Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    var length = 1000;
    Value vec = getMethod.apply("fibarr").execute(self, length, Integer.MAX_VALUE);

    switch (benchmarkName) {
      case "averageOverVector":
        {
          this.arrayOfFibNumbers = vec;
          break;
        }
      case "averageOverSlice":
        {
          this.arrayOfFibNumbers = getMethod.apply("slice").execute(self, 0, vec, 1, length);
          break;
        }
      case "averageOverSliceWrapped10":
        {
          this.arrayOfFibNumbers = getMethod.apply("slice").execute(self, 10, vec, 1, length);
          break;
        }
      case "averageOverSliceWrapped100":
        {
          this.arrayOfFibNumbers = getMethod.apply("slice").execute(self, 100, vec, 1, length);
          break;
        }
      case "averageOverArray":
        {
          this.arrayOfFibNumbers = getMethod.apply("to_array").execute(self, vec);
          break;
        }
      case "averageOverPolyglotVector":
        {
          long[] copy = copyToPolyglotArray(vec);
          this.arrayOfFibNumbers = getMethod.apply("to_vector").execute(self, copy);
          break;
        }
      case "averageOverPolyglotArray":
        {
          long[] copy = copyToPolyglotArray(vec);
          this.arrayOfFibNumbers = Value.asValue(copy);
          break;
        }
      case "averageOverArrayProxy":
        {
          this.arrayOfFibNumbers = getMethod.apply("create_array_proxy").execute(self, vec);
          break;
        }
      case "averageOverArrayProxyNew":
        {
          this.arrayOfFibNumbers = getMethod.apply("create_array_proxy_new").execute(self, vec);
          break;
        }
      case "averageAbstractList":
        {
          long[] copy = copyToPolyglotArray(vec);
          final ProxyList<Long> proxyList = new ProxyList<Long>();
          getMethod.apply("fill_proxy").execute(self, proxyList, copy);
          this.arrayOfFibNumbers = Value.asValue(proxyList);
          break;
        }

      default:
        throw new IllegalStateException("Unexpected benchmark: " + params.getBenchmark());
    }
    this.avg = getMethod.apply("avg");
  }

  private long[] copyToPolyglotArray(Value arr) {
    long[] copy = new long[Math.toIntExact(arr.getArraySize())];
    for (int i = 0; i < copy.length; i++) {
      copy[i] = arr.getArrayElement(i).asLong();
    }
    return copy;
  }

  @Benchmark
  public void averageOverVector(Blackhole matter) {
    performBenchmark(matter);
  }

  /** Measures performance of a single {@code Vector.slice}. */
  @Benchmark
  public void averageOverSlice(Blackhole matter) {
    performBenchmark(matter);
  }

  /** Measures performance of {@code Vector.slice} applied ten times on the same array. */
  @Benchmark
  public void averageOverSliceWrapped10(Blackhole matter) {
    performBenchmark(matter);
  }

  /** Measures performance of {@code Vector.slice} applied hundred times on the same array. */
  @Benchmark
  public void averageOverSliceWrapped100(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageOverPolyglotVector(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageOverArray(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageOverPolyglotArray(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageOverArrayProxy(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageOverArrayProxyNew(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void averageAbstractList(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole hole) throws AssertionError {
    var average = avg.execute(self, arrayOfFibNumbers);
    if (!average.fitsInDouble()) {
      throw new AssertionError("Shall be a double: " + average);
    }
    var result = (long) average.asDouble();
    boolean isResultCorrect =
        (result >= 1019950590 && result <= 1019950600)
            || (result >= 1020971561 && result <= 1020971571);
    if (!isResultCorrect) {
      throw new AssertionError(
          "Expecting reasonable average but was " + result + "\n" + arrayOfFibNumbers);
    }
    hole.consume(result);
  }

  public static final class ProxyList<T> extends AbstractList<T> {
    private Function<Object, Integer> size;
    private Function<Integer, T> get;

    public void init(Function<Object, Integer> size, Function<Integer, T> get) {
      this.size = size;
      this.get = get;
    }

    @Override
    public T get(int i) {
      return get.apply(i);
    }

    @Override
    public int size() {
      return size.apply(0);
    }
  }
}
