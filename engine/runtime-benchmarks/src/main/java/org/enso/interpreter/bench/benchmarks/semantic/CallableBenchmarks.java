package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
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

  private static final String TAIL_CALL_EXPLICIT = "@Tail_Call ";
  private final String TAIL_CALL_SUFFIX = "WithTailCall";
  private static final long HUNDRED_MILLION = 100_000_000L;
  private static final String SUM_TCO_FROM_CALL_CODE =
"""
from Standard.Base.Data.Numbers import all

type Foo
    Value v

Foo.from (that : Number) current=0 =
    if current == 0 then
        (Foo.Value that)
    else
        s = that + current
        c = current - 1
        @Tail_Call Foo.from s c

main = sumTo ->
    res = Foo.from 0 sumTo
    res
""";

  private static final String SUM_TCO_METHOD_CALL_CODE =
"""
summator = acc -> current ->
    if current == 0 then acc else
        s = acc + current
        c = current - 1
        @Tail_Call summator s c

main = sumTo ->
    res = summator 0 sumTo
    res
""";

  private static final String SUM_TCO_METHOD_CALL_WITH_NAMED_ARGUMENTS_CODE =
"""
summator = acc -> current ->
    if current == 0 then acc else
        s = acc + current
        c = current - 1
        @Tail_Call summator (current = c) (acc = s)

main = sumTo ->
    res = summator current=sumTo acc=0
    res
""";

  private static final String SUM_TCO_METHOD_CALL_WITH_DEFAULTED_ARGUMENTS_CODE =
"""
summator = (acc = 0) -> current ->
    if current == 0 then acc else
        s = acc + current
        c = current - 1
        @Tail_Call summator (current = c) (acc = s)

main = sumTo ->
    res = summator current=sumTo
    res
""";

  private ContextUtils ctxRule;
  private Value sumTCOfromCall;
  private Value sumTCOmethodCall;
  private Value sumTCOmethodCallWithNamedArguments;
  private Value sumTCOmethodCallWithDefaultedArguments;

  @Setup
  public void initializeBenchmarks(BenchmarkParams params) throws IOException {
    this.ctxRule = org.enso.compiler.benchmarks.Utils.createDefaultContextBuilder().build();

    this.sumTCOfromCall = getMethodFromModule(SUM_TCO_FROM_CALL_CODE, params);
    this.sumTCOmethodCall = getMethodFromModule(SUM_TCO_METHOD_CALL_CODE, params);
    this.sumTCOmethodCallWithNamedArguments =
        getMethodFromModule(SUM_TCO_METHOD_CALL_WITH_NAMED_ARGUMENTS_CODE, params);
    this.sumTCOmethodCallWithDefaultedArguments =
        getMethodFromModule(SUM_TCO_METHOD_CALL_WITH_DEFAULTED_ARGUMENTS_CODE, params);
  }

  @Benchmark
  public void benchSumTCOfromCall(Blackhole bh) {
    var res = sumTCOfromCall.execute(HUNDRED_MILLION).getMember("v");
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOfromCallWithTailCall(Blackhole bh) {
    benchSumTCOfromCall(bh);
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
  public void benchSumTCOmethodCallWithTailCall(Blackhole bh) {
    benchSumTCOmethodCall(bh);
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
  public void benchSumTCOmethodCallWithNamedArgumentsWithTailCall(Blackhole bh) {
    benchSumTCOmethodCallWithNamedArguments(bh);
  }

  @Benchmark
  public void benchSumTCOmethodCallWithDefaultedArguments(Blackhole bh) {
    var res = sumTCOmethodCallWithDefaultedArguments.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOmethodCallWithDefaultedArgumentsWithTailCall(Blackhole bh) {
    benchSumTCOmethodCallWithDefaultedArguments(bh);
  }

  private Value getMethodFromModule(String code, BenchmarkParams params) throws IOException {
    var benchName = params.getBenchmark();
    var tailCall = code.indexOf(TAIL_CALL_EXPLICIT);
    if (benchName.contains(TAIL_CALL_SUFFIX)) {
      if (tailCall == -1) {
        throw new AssertionError("There should be @Tail_Call in: " + code);
      }
    } else {
      var before = code.substring(0, tailCall);
      var after = code.substring(tailCall + TAIL_CALL_EXPLICIT.length(), code.length());
      code = before + after;
      var noTailCall = code.indexOf(TAIL_CALL_EXPLICIT);
      if (noTailCall != -1) {
        throw new AssertionError("No @Tail_Call anymore: " + code);
      }
    }
    return SrcUtil.getMainMethod(ctxRule, benchName, code);
  }
}
