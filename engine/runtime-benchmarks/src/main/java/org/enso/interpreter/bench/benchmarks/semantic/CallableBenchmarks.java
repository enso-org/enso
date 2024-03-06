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
public class CallableBenchmarks {
  private static final long HUNDRED_MILLION = 100_000_000L;
  private static final String SUM_TCO_FROM_CALL_CODE =
      """
from Standard.Base.Data.Numbers import all

type Foo

Foo.from (that : Number) current=0 =
    if current == 0 then that else @Tail_Call Foo.from (that + current) (current - 1)

main = sumTo ->
    res = Foo.from 0 sumTo
    res
""";

  private static final String SUM_TCO_METHOD_CALL_CODE =
      """
summator = acc -> current ->
    if current == 0 then acc else @Tail_Call summator (acc + current) (current - 1)

main = sumTo ->
    res = summator 0 sumTo
    res
""";

  private static final String SUM_TCO_METHOD_CALL_WITH_NAMED_ARGUMENTS_CODE =
      """
summator = acc -> current ->
    if current == 0 then acc else @Tail_Call summator (current = current - 1) (acc = acc + current)

main = sumTo ->
    res = summator current=sumTo acc=0
    res
""";

  private static final String SUM_TCO_METHOD_CALL_WITH_DEFAULTED_ARGUMENTS_CODE =
      """
summator = (acc = 0) -> current ->
    if current == 0 then acc else @Tail_Call summator (current = current - 1) (acc = acc + current)

main = sumTo ->
    res = summator current=sumTo
    res
""";

  private Context context;
  private Value sumTCOfromCall;
  private Value sumTCOmethodCall;
  private Value sumTCOmethodCallWithNamedArguments;
  private Value sumTCOmethodCallWithDefaultedArguments;

  @Setup
  public void initializeBenchmarks(BenchmarkParams params) {
    this.context = SrcUtil.newContextBuilder().build();

    this.sumTCOfromCall = Utils.getMainMethod(context, SUM_TCO_FROM_CALL_CODE);
    this.sumTCOmethodCall = Utils.getMainMethod(context, SUM_TCO_METHOD_CALL_CODE);
    this.sumTCOmethodCallWithNamedArguments =
        Utils.getMainMethod(context, SUM_TCO_METHOD_CALL_WITH_NAMED_ARGUMENTS_CODE);
    this.sumTCOmethodCallWithDefaultedArguments =
        Utils.getMainMethod(context, SUM_TCO_METHOD_CALL_WITH_DEFAULTED_ARGUMENTS_CODE);
  }

  @Benchmark
  public void benchSumTCOfromCall(Blackhole bh) {
    var res = sumTCOfromCall.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOmethodCall(Blackhole bh) {
    var res = sumTCOmethodCall.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOmethodCallWithNamedArguments(Blackhole bh) {
    var res = sumTCOmethodCallWithNamedArguments.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOmethodCallWithDefaultedArguments(Blackhole bh) {
    var res = sumTCOmethodCallWithDefaultedArguments.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }
}
