package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

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
          qualified_generator = acc -> i -> if i == 0 then acc else
              @Tail_Call qualified_generator (List.Cons i acc) (i - 1)

          res = qualified_generator List.Nil length
          res
      """;

  private static final String GENERATE_LIST_AUTOSCOPING_CODE =
      """
      import Standard.Base.Data.List.List
      import Standard.Base.Data.Numbers
      import Standard.Base.Data.Numbers.Integer

      main = length ->
          autoscoped_generator x i:Integer =
              acc = x:List
              if i == 0 then acc else
                c = ..Cons i acc
                i1 = i - 1
                @Tail_Call autoscoped_generator c i1

          res = autoscoped_generator ..Nil length
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
  private Value generateListAutoscoping;
  private Value reverseList;
  private Value reverseListMethods;
  private Value sumList;
  private Value sumListLeftFold;
  private Value sumListFallback;
  private Value sumListMethods;
  private Value mapReverseList;
  private Value mapReverseListCurry;

  @Setup
  public void initializeBenchmarks(BenchmarkParams params) throws IOException {
    this.context = SrcUtil.newContextBuilder().build();

    var millionElemListMethod = mainMethod(context, "millionElementList", MILLION_ELEMENT_LIST);
    this.millionElementsList = millionElemListMethod.execute();

    var lastDot = params.getBenchmark().lastIndexOf('.');
    var name = params.getBenchmark().substring(lastDot + 1);
    switch (name) {
      case "benchGenerateList" -> {
        this.generateList = mainMethod(context, name, GENERATE_LIST_CODE);
      }
      case "benchGenerateListQualified" -> {
        this.generateListQualified = mainMethod(context, name, GENERATE_LIST_QUALIFIED_CODE);
      }
      case "benchGenerateListAutoscoping" -> {
        this.generateListAutoscoping = mainMethod(context, name, GENERATE_LIST_AUTOSCOPING_CODE);
      }
      case "benchReverseList" -> {
        this.reverseList = mainMethod(context, name, REVERSE_LIST_CODE);
      }
      case "benchReverseListMethods" -> {
        this.reverseListMethods = mainMethod(context, name, REVERSE_LIST_METHODS_CODE);
      }
      case "benchSumList" -> {
        this.sumList = mainMethod(context, name, SUM_LIST_CODE);
      }
      case "benchSumListLeftFold" -> {
        this.sumListLeftFold = mainMethod(context, name, SUM_LIST_LEFT_FOLD_CODE);
      }
      case "benchSumListFallback" -> {
        this.sumListFallback = mainMethod(context, name, SUM_LIST_FALLBACK_CODE);
      }
      case "benchSumListMethods" -> {
        this.sumListMethods = mainMethod(context, name, SUM_LIST_METHODS_CODE);
      }
      case "benchMapReverseList" -> {
        this.mapReverseList = mainMethod(context, name, MAP_REVERSE_LIST_CODE);
      }
      case "benchMapReverseListCurry" -> {
        this.mapReverseListCurry = mainMethod(context, name, MAP_REVERSE_LIST_CURRY_CODE);
      }
      default -> throw new IllegalArgumentException(name);
    }
  }

  private static Value mainMethod(Context context, String name, String code) throws IOException {
    return SrcUtil.getMainMethod(context, name, code);
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
  public void benchGenerateListAutoscoping(Blackhole bh) {
    var res = generateListAutoscoping.execute(MILLION);
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
  public void benchSumListLeftFold(Blackhole bh) {
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
  public void benchMapReverseListCurry(Blackhole bh) {
    var res = mapReverseListCurry.execute(millionElementsList);
    bh.consume(res);
  }
}
