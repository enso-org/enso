package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.IOException;
import java.util.Objects;
import org.enso.common.MethodNames;
import org.enso.compiler.benchmarks.Utils;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.infra.BenchmarkParams;

/**
 * Base class for {@link FibHostJavaPolyglotBenchmarks} and {@link FibGuestJavaPolyglotBenchmarks}.
 */
public abstract class FibBenchmarks {

  private ContextUtils ctx;
  private Value fib;

  protected Context.Builder withModifiedContext(Context.Builder b) {
    return b;
  }

  @Setup()
  public void initializeBench(BenchmarkParams params) throws IOException {
    ctx =
        Utils.createDefaultContextBuilder().withModifiedContext(this::withModifiedContext).build();

    var code =
        """
        polyglot java import java.lang.Math

        main n=10 =
            fib n

        private fib n = if Math.max n 1 == 1 then 1 else
            n1 = Math.decrementExact n
            n2 = Math.decrementExact n1

            f1 = fib n1
            f2 = fib n2

            Math.addExact f1 f2
        """;

    var benchmarkName = SrcUtil.findName(params);
    var src = SrcUtil.source(benchmarkName, code);
    var module = ctx.eval(src);
    fib = Objects.requireNonNull(module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fib"));
  }

  @TearDown
  public void tearDown() {
    ctx.close();
  }

  @Benchmark
  public void fib10() {
    runBench(10, 89);
  }

  @Benchmark
  public void fib17() {
    runBench(17, 2584);
  }

  @Benchmark
  public void fib21() {
    runBench(21, 17711);
  }

  /** Call this method to perform the benchmark of certain magnitude. */
  protected final void runBench(long value, long expValue) {
    var res = fib.execute(value);
    if (res.asLong() != expValue) {
      throw new AssertionError(
          "Expected result for fib(%d) is %d, but got %d".formatted(value, expValue, res.asInt()));
    }
  }
}
