package org.enso.interpreter.bench.benchmarks.semantic;

import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.interpreter.bench.Utils;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
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
public class AtomBenchmarks {

  private static final Long MILLION = 1_000_000L;
  private static final String MILLION_ELEMENT_LIST =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main =
          generator fn acc i end = if i == end then acc else @Tail_Call generator fn (fn acc i) i+1 end
          res = generator (acc -> x -> List.Cons x acc) List.Nil 1 $million
          res
      """
          .replace("$million", MILLION.toString());

  private static final String GENERATE_LIST_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main = length ->
          generator = acc -> i -> if i == 0 then acc else @Tail_Call generator (List.Cons i acc) (i - 1)

          res = generator List.Nil length
          res
      """;
  private static final String GENERATE_LIST_QUALIFIED_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main = length ->
          generator = acc -> i -> if i == 0 then acc else @Tail_Call generator (List.Cons i acc) (i - 1)

          res = generator List.Nil length
          res
      """;
  private static final String REVERSE_LIST_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main = list ->
          reverser = acc -> list -> case list of
              List.Cons h t -> @Tail_Call reverser (List.Cons h acc) t
              List.Nil -> acc

         res = reverser List.Nil list
         res
      """;
  private static final String REVERSE_LIST_METHODS_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      List.rev self acc = case self of
          List.Cons h t -> @Tail_Call t.rev (List.Cons h acc)
          _ -> acc

      main = list ->
          res = list.rev List.Nil
          res
      """;
  private static final String SUM_LIST_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main = list ->
          summator = acc -> list -> case list of
              List.Cons h t -> @Tail_Call summator acc+h t
              List.Nil -> acc

         res = summator 0 list
         res
      """;
  private static final String SUM_LIST_LEFT_FOLD_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main = list ->
          fold = f -> acc -> list -> case list of
              List.Cons h t -> @Tail_Call fold f (f acc h) t
              _ -> acc

          res = fold (x -> y -> x + y) 0 list
          res
      """;
  private static final String SUM_LIST_FALLBACK_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      main = list ->
          summator = acc -> list -> case list of
              List.Cons h t -> @Tail_Call summator acc+h t
              _ -> acc

          res = summator 0 list
          res
      """;
  private static final String SUM_LIST_METHODS_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      List.sum self acc = case self of
          List.Cons h t -> @Tail_Call t.sum h+acc
          _ -> acc

      main = list ->
          res = list.sum 0
          res
      """;
  private static final String MAP_REVERSE_LIST_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      List.mapReverse self f acc = case self of
          List.Cons h t -> @Tail_Call t.mapReverse f (List.Cons (f h) acc)
          _ -> acc

      main = list ->
          res = list.mapReverse (x -> x + 1) List.Nil
          res
      """;
  private static final String MAP_REVERSE_LIST_CURRY_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers

      List.mapReverse self f acc = case self of
          List.Cons h t -> @Tail_Call t.mapReverse f (List.Cons (f h) acc)
          _ -> acc

      main = list ->
          adder = x -> y -> x + y
          res = list.mapReverse (adder 1) List.Nil
          res
      """;

  private Context context;
  private Value millionElementsList;
  private Value generateList;
  private Value generateListQualified;
  private Value reverseList;
  private Value reverseListMethods;
  private Value sumList;
  private Value sumListLeftFold;
  private Value sumListFallback;
  private Value sumListMethods;
  private Value mapReverseList;
  private Value mapReverseListCurry;

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

    var millionElemListMethod = Utils.getMainMethod(context, MILLION_ELEMENT_LIST);
    this.millionElementsList = millionElemListMethod.execute();
    this.generateList = Utils.getMainMethod(context, GENERATE_LIST_CODE);
    this.generateListQualified = Utils.getMainMethod(context, GENERATE_LIST_QUALIFIED_CODE);
    this.reverseList = Utils.getMainMethod(context, REVERSE_LIST_CODE);
    this.reverseListMethods = Utils.getMainMethod(context, REVERSE_LIST_METHODS_CODE);
    this.sumList = Utils.getMainMethod(context, SUM_LIST_CODE);
    this.sumListLeftFold = Utils.getMainMethod(context, SUM_LIST_LEFT_FOLD_CODE);
    this.sumListFallback = Utils.getMainMethod(context, SUM_LIST_FALLBACK_CODE);
    this.sumListMethods = Utils.getMainMethod(context, SUM_LIST_METHODS_CODE);
    this.mapReverseList = Utils.getMainMethod(context, MAP_REVERSE_LIST_CODE);
    this.mapReverseListCurry = Utils.getMainMethod(context, MAP_REVERSE_LIST_CURRY_CODE);
  }

  @Benchmark
  public void benchGenerateList(Blackhole bh) {
    var res = generateList.execute(MILLION);
    bh.consume(res);
  }

  @Benchmark
  public void benchGenerateListQualified(Blackhole bh) {
    var res = generateListQualified.execute(MILLION);
    bh.consume(res);
  }

  @Benchmark
  public void benchReverseList(Blackhole bh) {
    var reversedList = reverseList.execute(millionElementsList);
    bh.consume(reversedList);
  }

  @Benchmark
  public void benchReverseListMethods(Blackhole bh) {
    var reversedList = reverseListMethods.execute(millionElementsList);
    bh.consume(reversedList);
  }

  @Benchmark
  public void benchSumList(Blackhole bh) {
    var res = sumList.execute(millionElementsList);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return a number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void sumListLeftFold(Blackhole bh) {
    var res = sumListLeftFold.execute(millionElementsList);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return a number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumListFallback(Blackhole bh) {
    var res = sumListFallback.execute(millionElementsList);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return a number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchSumListMethods(Blackhole bh) {
    var res = sumListMethods.execute(millionElementsList);
    if (!res.fitsInLong()) {
      throw new AssertionError("Should return a number");
    }
    bh.consume(res);
  }

  @Benchmark
  public void benchMapReverseList(Blackhole bh) {
    var res = mapReverseList.execute(millionElementsList);
    bh.consume(res);
  }

  @Benchmark
  public void benchMapReverseCurryList(Blackhole bh) {
    var res = mapReverseListCurry.execute(millionElementsList);
    bh.consume(res);
  }
}
