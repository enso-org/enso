package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.regex.Pattern;
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
@Warmup(iterations = 2, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
public class VectorBuilderBenchmarks {
  private Value create;
  private Value self;
  private int length;
  private Object value;
  private Object specialValue;
  private int at;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Utils.createDefaultContextBuilder().build();
    var benchmarkName = SrcUtil.findName(params);
    var code =
        """
        from Standard.Base import all

        create value size:Integer special_value at =
            Vector.build b->
                0.up_to size . each index->
                    if index == at then b.append special_value else
                        b.append value
        """;

    var module = ctx.eval(SrcUtil.source(benchmarkName, code));

    this.self = module.invokeMember("get_associated_type");
    Function<String, Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    this.length = Integer.parseInt(regex(".*Length([0-9]+).*", benchmarkName, null));
    var valueType = regex(".*Type([A-Z][a-z]+).*", benchmarkName, null);
    this.value = valueFor(valueType);
    this.specialValue = valueFor(regex(".*With([A-Z][a-z]*).*", benchmarkName, valueType));
    this.at = Integer.parseInt(regex(".*At([0-9]+).*", benchmarkName, "-1"));
    this.create = getMethod.apply("create");
  }

  @Benchmark
  public void buildTypeIntegerLength5(Blackhole matter) {
    // default size of Vector.build is 10
    // e.g. this benchmark fits into initial size
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeIntegerLength15(Blackhole matter) {
    // default size of Vector.build is 10
    // e.g. this benchmark exceeds initial size
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeIntegerLength150(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeIntegerLength1500(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeFloatLength15(Blackhole matter) {
    // default size of Vector.build is 10
    // e.g. this benchmark exceeds initial size
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeFloatLength150(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeFloatLength1500(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeTextLength15(Blackhole matter) {
    // default size of Vector.build is 10
    // e.g. this benchmark exceeds initial size
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeTextLength150(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeTextLength1500(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeIntegerLength5WithFloatAt2(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeIntegerLength15WithFloatAt2(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void buildTypeIntegerLength150WithFloatAt2(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole hole) throws AssertionError {
    var result = create.execute(self, value, length, specialValue, at);
    if (!result.hasArrayElements()) {
      throw new IllegalStateException("Expecting array, but got: " + result);
    }
    var realLength = result.getArraySize();
    if (realLength != length) {
      throw new IllegalStateException(
          "Expecting array size " + length + " , but got: " + realLength);
    }
    hole.consume(result);
  }

  private static String regex(String pattern, String benchmarkName, String def) {
    var matcher = Pattern.compile(pattern).matcher(benchmarkName);
    if (!matcher.matches()) {
      if (def != null) {
        return def;
      }
      throw new IllegalStateException("No match of " + pattern + " in " + benchmarkName);
    }
    return matcher.group(1);
  }

  private static Object valueFor(String type) throws IllegalStateException {
    return switch (type) {
      case "Integer" -> 42L;
      case "Float" -> Math.PI;
      case "Text" -> "Hello";
      case "Nothing" -> null;
      case Object unknown -> throw new IllegalStateException("Unknwon value type " + unknown);
    };
  }
}
