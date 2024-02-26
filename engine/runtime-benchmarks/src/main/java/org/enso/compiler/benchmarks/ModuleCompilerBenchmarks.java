package org.enso.compiler.benchmarks;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.enso.compiler.Compiler;
import org.enso.interpreter.runtime.Module;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.interpreter.runtime.data.Type;
import org.graalvm.polyglot.Source;
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

/**
 * Measures how long does it take for the compiler passes to run on a various modules.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(0)
@Warmup(iterations = 1)
@Measurement(iterations = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ModuleCompilerBenchmarks {
  private Compiler compiler;
  private Module module;

  @Setup
  public void setup(BenchmarkParams params) throws IOException {
    var ctx = Utils.createDefaultContext();
    var ensoCtx = Utils.leakEnsoContext(ctx);
    switch (params.getBenchmark()) {

      default -> {
        throw new UnsupportedOperationException("unimplemented: Benchmark " + params.getBenchmark());
      }
    }
  }


  /**
   * Measure compilation of a module with a lot of small methods with
   * variable number of arguments.
   * @param blackhole
   */
  @Benchmark
  public void manySmallMethods(Blackhole blackhole) {
    var compilerResult = compiler.run(module.asCompilerModule());
    if (compilerResult.compiledModules().size() != 1) {
      throw new AssertionError("Module compilation failed");
    }
  }

  /**
   * Measure compilation of a module with one method that contains a lot of
   * nested blocks.
   * @param blackhole
   */
  @Benchmark
  public void manyNestedBlocks(Blackhole blackhole) {
    throw new UnsupportedOperationException("unimplemented");
  }

  /**
   * Measure compilation of a module with one method that contains a lot of errors -
   * syntactical errors and unknown identifiers. The compiler should be able to recover from errors
   * and so it should compile the whole module and not stop after the first error.
   * @param blackhole
   */
  @Benchmark
  public void manyErrors(Blackhole blackhole) {
    throw new UnsupportedOperationException("unimplemented");
  }
}
