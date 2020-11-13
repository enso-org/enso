package org.enso.syntax;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 5)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class LexerBench {

  @State(Scope.Thread)
  public static class BenchState {
    @Param({"1024" /* 1KB */, "102400" /* 100KB */, "1048576" /* 1MB */, "10485760" /* 10MB */})
    public int bytesSize;

    public String myInput;

    @Setup(Level.Trial)
    public void doSetup(BenchmarkParams params) {
      var benchNameSegments = params.getBenchmark().split("\\.");
      var benchName = benchNameSegments[benchNameSegments.length - 1];
      var benchInput = LexerBenchFixtures.benchmarks().get(benchName).get();
      this.myInput = LexerBenchFixtures.replicate(benchInput, bytesSize, false);
    }
  }


  // === Literals ===

  @Benchmark
  public void literalNumberInteger(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalNumberIntegerExplicitBase(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalNumberDecimal(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalNumberDecimalExplicitBase(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalNumberErrorBase(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalTextFormatLine(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalTextFormatInlineBlock(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalTextFormatBlock(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalTextRawLine(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalRawInlineBlock(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void literalRawBlock(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }


  // === Names ===

  @Benchmark
  public void nameLineOf(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void nameInvalidSuffix(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }


  // === Operators ===

  @Benchmark
  public void operatorLineOf(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void operatorDotCall(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void operatorInvalidSuffix(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }


  // === Blocks ===

  @Benchmark
  public void blockTopLevel(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void blockNested(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void blockDeeplyNested(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }


  // === Comments ===

  @Benchmark
  public void commentLine(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void commentInLine(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void commentDoc(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }


  // === Combined ===

  @Benchmark
  public void combinedSimple(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }

  @Benchmark
  public void combinedComplex(Blackhole blackhole, BenchState state) {
    blackhole.consume(LexerBenchFixtures.runLexer(state.myInput));
  }
}
