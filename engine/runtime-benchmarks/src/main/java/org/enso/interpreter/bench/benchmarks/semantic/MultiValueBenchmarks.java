package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.concurrent.TimeUnit;
import org.enso.common.MethodNames;
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

/**
 * These benchmarks compare performance of {@link EnsoMultiValue}. They create a vector in a certain
 * configuration representing numbers and then they perform {@code sum} operation on it.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class MultiValueBenchmarks {
  private Value arrayOfNumbers;
  private Value sum;
  private Value self;
  private final long length = 100000;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    var ctx = Utils.createDefaultContextBuilder().build();
    var code =
        """
        from Standard.Base import Vector, Float, Number, Integer

        type Complex
            private Number re:Float im:Float

        Complex.from (that:Number) = Complex.Number that 0

        sum arr =
            go acc i = if i >= arr.length then acc else
                v = arr.at i
                sum = acc + v
                @Tail_Call go sum i+1
            go 0 0

        sum_re arr =
            go acc i = if i >= arr.length then acc else
                v = arr.at i . re
                sum = acc + v
                @Tail_Call go sum i+1
            go 0 0

        make_vector typ n =
            Vector.new n i->
                r = 3 + 5*i
                case typ of
                    0 -> r:Complex
                    1 -> r:Integer
                    2 -> r:Float
                    3 ->
                        c = r:Complex&Float
                        c:Float
                    4 ->
                        c = r:Float&Complex
                        c:Float
                    5 -> r:Complex&Float
                    6 -> r:Float&Complex
        """;
    var benchmarkName = SrcUtil.findName(params);
    var src = SrcUtil.source(benchmarkName, code);
    int type = Integer.parseInt(benchmarkName.substring(benchmarkName.length() - 1));

    var module = ctx.eval(src);
    this.self = module.invokeMember(MethodNames.Module.GET_ASSOCIATED_TYPE);
    var makeVector = module.invokeMember("get_method", self, "make_vector");
    this.arrayOfNumbers = makeVector.execute(self, type, length);
    this.sum =
        module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, type == 0 ? "sum_re" : "sum");
  }

  /**
   * The <b>base benchmark</b> for this suite. Measures how much it takes to access an Atom in a
   * Vector, read {@code re:Float} field out of it and sum all of them together.
   */
  @Benchmark
  public void sumOverComplexBaseBenchmark0(Blackhole matter) {
    performBenchmark(matter);
  }

  /**
   * Working with {@code Integer} should be the fastest. The plus operation on integer are faster
   * than those on {@code Float} and moreover the {@code Vector} has a special representation when
   * full of {@code long} values.
   */
  @Benchmark
  public void sumOverInteger1(Blackhole matter) {
    performBenchmark(matter);
  }

  /**
   * Working with {@code Float} should be also fast. The {@code Vector} has a special representation
   * when full of {@code double} values that increases cache locality.
   */
  @Benchmark
  public void sumOverFloat2(Blackhole matter) {
    performBenchmark(matter);
  }

  //
  // Following benchmarks shall catch up with the base benchmark
  //

  @Benchmark
  public void sumOverComplexFloatRecastedToFloat3(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverFloatComplexRecastedToFloat4(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverComplexAndFloat5(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverFloatAndComplex6(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole matter) throws AssertionError {
    var resultValue = sum.execute(arrayOfNumbers);
    if (!resultValue.fitsInLong()) {
      throw new AssertionError("Shall be a long: " + resultValue);
    }
    long result = resultValue.asLong();
    long expectedResult = length * 3L + (5L * (length * (length - 1L) / 2L));
    boolean isResultCorrect = result == expectedResult;
    if (!isResultCorrect) {
      throw new AssertionError("Expecting " + expectedResult + " but was " + result);
    }
    matter.consume(result);
  }
}
