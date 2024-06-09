package org.enso.interpreter.bench.benchmarks.semantic;

import java.util.concurrent.TimeUnit;
import org.enso.interpreter.bench.Utils;
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
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class NamedDefaultedArgumentBenchmarks {
  private static long HUNDRED_MILLION = 100_000_000L;
  private static final String SUM_TCO_WITH_NAMED_ARGUMENTS_CODE =
      """
import Standard.Base.Data.Numbers
main = sumTo ->
    summator = acc -> current ->
        if current == 0 then acc else @Tail_Call summator (current = current - 1) (acc = acc + current)

    res = summator current=sumTo acc=0
    res
""";

  private static final String SUM_TCO_WITH_DEFAULTED_ARGUMENTS_CODE =
      """
import Standard.Base.Data.Numbers
main = sumTo ->
    summator = (acc = 0) -> current ->
        if current == 0 then acc else @Tail_Call summator (current = current - 1) (acc = acc + current)

    res = summator (current = sumTo)
    res
""";

  private Context context;
  private Value sumTCOWithNamedArguments;
  private Value sumTCOWithDefaultedArguments;

  @Setup
  public void initializeBenchmarks(BenchmarkParams params) {
    this.context = org.enso.compiler.benchmarks.Utils.createDefaultContextBuilder().build();
    this.sumTCOWithNamedArguments = Utils.getMainMethod(context, SUM_TCO_WITH_NAMED_ARGUMENTS_CODE);
    this.sumTCOWithDefaultedArguments =
        Utils.getMainMethod(context, SUM_TCO_WITH_DEFAULTED_ARGUMENTS_CODE);
  }

  @Benchmark
  public void benchSumTCOWithNamedArgs(Blackhole bh) {
    var res = sumTCOWithNamedArguments.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOWithDefaultArgs(Blackhole bh) {
    var res = sumTCOWithDefaultedArguments.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }
}
