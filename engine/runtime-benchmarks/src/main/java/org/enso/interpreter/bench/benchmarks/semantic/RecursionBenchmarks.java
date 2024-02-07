package org.enso.interpreter.bench.benchmarks.semantic;

import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.interpreter.bench.Utils;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 5)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class RecursionBenchmarks {
  private static final String SUM_TCO_CODE =
      """
main = sumTo ->
    summator = acc -> current ->
        if current == 0 then acc else @Tail_Call summator acc+current current-1

    res = summator 0 sumTo
    res
""";

  private static final String SUM_TCO_FOLD_LIKE_CODE =
      """
main = sumTo ->
    summator = acc -> i -> f ->
        if i == 0 then acc else @Tail_Call summator (f acc i) (i - 1) f
    res = summator 0 sumTo (x -> y -> x + y)
    res
""";

  private static final String SUM_RECURSIVE_CODE =
      """
main = sumTo ->
    summator = i -> if i == 0 then 0 else i + summator (i - 1)
    res = summator sumTo
    res
""";

  private static final String OVERSATURATED_RECURSIVE_CALL_TCO_CODE =
      """
main = sumTo ->
    summator = acc -> i -> f ->
        if i == 0 then acc else @Tail_Call summator (f acc i) (i - 1) f
    res = summator 0 sumTo (x -> y -> x + y)
    res
""";

  private static final String SUM_STATE_TCO_CODE =
      """
from Standard.Base.Data.Numbers import Number
import Standard.Base.Runtime.State

stateSum = n ->
    acc = State.get Number
    State.put Number (acc + n)
    if n == 0 then State.get Number else @Tail_Call stateSum (n - 1)

main = sumTo ->
    res = State.run Number 0 (stateSum sumTo)
    res
""";

  private static final String SUM_TCO_WITH_EVAL_CODE =
      """
import Standard.Base.Runtime.Debug

main = sumTo ->
    summator = acc -> current ->
        if current == 0 then acc else Debug.eval "@Tail_Call summator (acc + current) (current - 1)"

   res = summator 0 sumTo
   res
""";

  private static final String NESTED_THUNK_SUM_CODE =
      """
from Standard.Base.Data.Numbers import Number
import Standard.Base.Runtime.State
import Standard.Base.Nothing.Nothing

doNTimes = n -> ~block ->
    block
    if n == 1 then Nothing else @Tail_Call doNTimes n-1 block

main = n ->
    block =
        x = State.get Number
        State.put Number x+1

    res = State.run Number 0 (doNTimes n block)
    res
""";
  private static final long HUNDRED_MILLION = 100_000_000L;
  private static final long HUNDRED = 100L;

  private Context context;
  private Value sumTCO;
  private Value sumTCOWithEval;
  private Value sumTCOFoldLike;
  private Value sumRecursive;
  private Value oversaturatedRecursiveCall;
  private Value sumStateTCO;
  private Value nestedThunkSum;

  @Setup
  public void initializeBenchmarks(BenchmarkParams params) {
    this.context =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();

    this.sumTCO = Utils.getMainMethod(context, SUM_TCO_CODE);
    this.sumTCOWithEval = Utils.getMainMethod(context, SUM_TCO_WITH_EVAL_CODE);
    this.sumTCOFoldLike = Utils.getMainMethod(context, SUM_TCO_FOLD_LIKE_CODE);
    this.sumRecursive = Utils.getMainMethod(context, SUM_RECURSIVE_CODE);
    this.oversaturatedRecursiveCall =
        Utils.getMainMethod(context, OVERSATURATED_RECURSIVE_CALL_TCO_CODE);
    this.sumStateTCO = Utils.getMainMethod(context, SUM_STATE_TCO_CODE);
    this.nestedThunkSum = Utils.getMainMethod(context, NESTED_THUNK_SUM_CODE);
  }

  private Value runOnHundredMillion(Value function) {
    var res = function.execute(HUNDRED_MILLION);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number, but got " + res);
    }
    return res;
  }

  @Benchmark
  public void benchSumTCO(Blackhole bh) {
    var res = runOnHundredMillion(sumTCO);
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOWithEval(Blackhole bh) {
    var res = runOnHundredMillion(sumTCOWithEval);
    bh.consume(res);
  }

  @Benchmark
  public void benchSumTCOFoldLike(Blackhole bh) {
    var res = runOnHundredMillion(sumTCOFoldLike);
    bh.consume(res);
  }

  @Benchmark
  public void benchSumRecursive(Blackhole bh) {
    var res = sumRecursive.execute(HUNDRED);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return number, but got " + res);
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchOversaturatedRecursiveCall(Blackhole bh) {
    var res = runOnHundredMillion(oversaturatedRecursiveCall);
    bh.consume(res);
  }

  @Benchmark
  public void benchSumStateTCO(Blackhole bh) {
    var res = runOnHundredMillion(sumStateTCO);
    bh.consume(res);
  }

  @Benchmark
  public void benchNestedThunkSum(Blackhole bh) {
    var res = nestedThunkSum.execute(HUNDRED_MILLION);
    if (!res.isNull()) {
      throw new AssertionError("Should return Nothing, but got " + res);
    }
    bh.consume(res);
  }
}
