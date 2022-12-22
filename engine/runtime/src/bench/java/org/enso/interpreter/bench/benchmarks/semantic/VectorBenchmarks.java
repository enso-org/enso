    package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.AbstractList;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
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
@Warmup(iterations = 3)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class VectorBenchmarks {
  private Value arrayOfFibNumbers;
  private Value avg;
  private Value self;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Context.newBuilder()
      .allowExperimentalOptions(true)
      .allowIO(true)
      .allowAllAccess(true)
      .logHandler(new ByteArrayOutputStream())
      .option(
        "enso.languageHomeOverride",
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();

    var benchmarkName = params.getBenchmark().replaceFirst(".*\\.", "");
    var code = """
        import Standard.Base.Data.Vector.Vector
        import Standard.Base.Data.Array_Proxy.Array_Proxy

        avg arr =
            sum acc i = if i == arr.length then acc else
                @Tail_Call sum (acc + arr.at i) i+1
            (sum 0 0) / arr.length

        fibarr size modulo =
            b = Vector.new_builder size
            b.append 1
            b.append 1

            add_more n = if n == size then b else
                b.append <| (b.at n-1 + b.at n-2) % modulo
                @Tail_Call add_more n+1

            add_more 2 . to_vector

        to_vector arr = Vector.from_polyglot_array arr
        to_array vec = vec.to_array
        slice vec = vec.slice
        fill_proxy proxy vec =
          size v = vec.length
          at i = vec.at i
          proxy.init size at
        create_array_proxy vec =
          Array_Proxy.from_proxy_object vec
        create_array_proxy_new vec =
          Array_Proxy.new vec.length (i -> vec.at i)
        """;

    var module = ctx.eval(SrcUtil.source(benchmarkName, code));

    this.self = module.invokeMember("get_associated_type");
    Function<String,Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    var length = 1000;
    Value vec = getMethod.apply("fibarr").execute(self, length, Integer.MAX_VALUE);

    switch (benchmarkName) {
      case "averageOverVector": {
        this.arrayOfFibNumbers = vec;
        break;
      }
      case "averageOverSlice": {
        this.arrayOfFibNumbers = getMethod.apply("slice").execute(self, vec, 1, length);
        break;
      }
      case "averageOverArray": {
        this.arrayOfFibNumbers = getMethod.apply("to_array").execute(self, vec);
        break;
      }
      case "averageOverPolyglotVector": {
        long[] copy = copyToPolyglotArray(vec);
        this.arrayOfFibNumbers = getMethod.apply("to_vector").execute(self, copy);
        break;
      }
      case "averageOverPolyglotArray": {
        long[] copy = copyToPolyglotArray(vec);
        this.arrayOfFibNumbers = Value.asValue(copy);
        break;
      }
      case "averageOverArrayProxy": {
        this.arrayOfFibNumbers = getMethod.apply("create_array_proxy").execute(self, vec);
        break;
      }
      case "averageOverArrayProxyNew": {
        this.arrayOfFibNumbers = getMethod.apply("create_array_proxy_new").execute(self, vec);
        break;
      }
      case "averageAbstractList": {
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

  @Benchmark
  public void averageOverSlice(Blackhole matter) {
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
    boolean isResultCorrect = (result >= 1019950590 && result <= 1019950600) || (result >= 1020971561 && result <= 1020971571);
    if (!isResultCorrect) {
      throw new AssertionError("Expecting reasonable average but was " + result + "\n" + arrayOfFibNumbers);
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

