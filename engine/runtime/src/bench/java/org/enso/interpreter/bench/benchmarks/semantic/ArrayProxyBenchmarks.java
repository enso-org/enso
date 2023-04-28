package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

/**
 * These benchmarks compare performance of ArrayProxy generating its values ad-hoc versus a vector
 * of pregenerated values. It assumes a very simple generation algorithm (like would be used to
 * generate Range.to_vector for example), as obviously with a costly generation logic access will
 * definitely not be faster than a memory-cached approach.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 3)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ArrayProxyBenchmarks {
  private Value arrayOfNumbers;
  private Value sum;
  private Value self;
  private final long length = 100000;

  @Setup
  public void initializeBenchmark(BenchmarkParams params) throws Exception {
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .logHandler(new ByteArrayOutputStream())
            .option(
                "enso.languageHomeOverride",
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    var ctx = Context.newBuilder().engine(eng).allowIO(true).allowAllAccess(true).build();
    var code = """
        import Standard.Base.Data.Vector.Vector
        import Standard.Base.Data.Array_Proxy.Array_Proxy
        sum arr =
            go acc i = if i >= arr.length then acc else
                @Tail_Call go (acc + arr.at i) i+1
            go 0 0

        make_vector n =
            Vector.new n (i -> 3 + 5*i)
        make_computing_proxy n =
            Array_Proxy.new n (i -> 3 + 5*i)
        make_delegating_proxy n =
            Array_Proxy.from_proxy_object (make_vector n)
        make_computing_vector n =
            Vector.from_polyglot_array (make_computing_proxy n)
        make_delegating_vector n =
            Vector.from_polyglot_array (make_delegating_proxy n)
        """;
    var benchmarkName = SrcUtil.findName(params);
    var src = SrcUtil.source(benchmarkName, code);
    var module = ctx.eval(src);

    this.self = module.invokeMember("get_associated_type");
    Function<String, Value> getMethod = (name) -> module.invokeMember("get_method", self, name);

    String test_builder;
    switch (benchmarkName) {
      case "sumOverVector":
        test_builder = "make_vector";
        break;
      case "sumOverComputingProxy":
        test_builder = "make_computing_proxy";
        break;
      case "sumOverDelegatingProxy":
        test_builder = "make_delegating_proxy";
        break;
      case "sumOverVectorBackedByComputingProxy":
        test_builder = "make_computing_vector";
        break;
      case "sumOverVectorBackedByDelegatingProxy":
        test_builder = "make_delegating_vector";
        break;
      default:
        throw new IllegalStateException(
            "Unexpected benchmark: " + params.getBenchmark());
    }
    this.arrayOfNumbers = getMethod.apply(test_builder).execute(self, length);
    this.sum = getMethod.apply("sum");
  }

  @Benchmark
  public void sumOverVector(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverComputingProxy(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverDelegatingProxy(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverVectorBackedByComputingProxy(Blackhole matter) {
    performBenchmark(matter);
  }

  @Benchmark
  public void sumOverVectorBackedByDelegatingProxy(Blackhole matter) {
    performBenchmark(matter);
  }

  private void performBenchmark(Blackhole matter) throws AssertionError {
    var resultValue = sum.execute(self, arrayOfNumbers);
    if (!resultValue.fitsInLong()) {
      throw new AssertionError("Shall be a long: " + resultValue);
    }
    long result = resultValue.asLong();
    long expectedResult = length * 3L + (5L * (length * (length - 1L) / 2L));
    boolean isResultCorrect = result == expectedResult;
    if (!isResultCorrect) {
      throw new AssertionError(
          "Expecting " + expectedResult + " but was " + result);
    }
    matter.consume(result);
  }
}
