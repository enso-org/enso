package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.IOException;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import org.enso.common.MethodNames.Module;
import org.enso.compiler.benchmarks.Utils;
import org.enso.test.utils.ContextUtils;
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
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 2, time = 5)
@Measurement(iterations = 3, time = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class FibHostJavaPolyglotBenchmarks {

  private ContextUtils ctx;
  private Value fib;

  @Setup()
  public void initializeBench(BenchmarkParams params) throws IOException {
    ctx = Utils.createDefaultContextBuilder().build();

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
    fib = Objects.requireNonNull(module.invokeMember(Module.EVAL_EXPRESSION, "fib"));
  }

  @TearDown
  public void tearDown() {
    ctx.close();
  }

  @Benchmark
  public void fib17() {
    runBench(17, 2584);
  }

  @Benchmark
  public void fib21() {
    runBench(21, 17711);
  }

  @Benchmark
  public void fib27() {
    runBench(27, 317811);
  }

  @Benchmark
  public void fib33() {
    runBench(33, 5702887);
  }

  private void runBench(long value, long expValue) {
    var res = fib.execute(value);
    if (res.asLong() != expValue) {
      throw new AssertionError(
          "Expected result for fib(%d) is %d, but got %d".formatted(value, expValue, res.asInt()));
    }
  }
}
