package org.enso.compiler.benchmarks.inline;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.Utils;
import org.graalvm.polyglot.Context;
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
import org.openjdk.jmh.infra.Blackhole;

/**
 * Measures the inline compilation, that is the compilation that is requested inside a method.
 * Simulates a scenario where there is an existing method and we are trying to insert a new
 * expression into it.
 *
 * <p>There is just a single `main` method that contains {@link #LOCAL_VARS_CNT} local variables.
 * One benchmark measures the time it takes to inline compile a long expression that uses all the
 * local variables. The other benchmark measures the time it takes to inline compile an expression
 * that contains some undefined identifiers and thus, should fail to compile.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class InlineCompilerBenchmark {
  /** How many variables should be declared in the main method. */
  private static final int LOCAL_VARS_CNT = 40;

  private static final int LONG_EXPR_SIZE = 5;

  private final OutputStream out = new ByteArrayOutputStream();
  private Compiler compiler;
  private Context ctx;
  private InlineSource inlineSource;
  private String longExpression;
  private Set<String> localVarNames;

  @Setup
  public void setup() throws IOException {
    ctx = Utils.createDefaultContextBuilder().out(out).err(out).logHandler(out).build();
    var ensoCtx = Utils.leakEnsoContext(ctx);
    compiler = ensoCtx.getCompiler();
    localVarNames = InlineContextUtils.localVarNames(LOCAL_VARS_CNT);
    longExpression = InlineContextUtils.createLongExpression(localVarNames, LONG_EXPR_SIZE);
    inlineSource = InlineContextUtils.createMainMethodWithLocalVars(ctx, localVarNames);
  }

  @TearDown
  public void tearDown() {
    ctx.close();
    try {
      if (!out.toString().isEmpty()) {
        throw new AssertionError("Unexpected output from the compiler: " + out);
      }
      out.close();
    } catch (IOException e) {
      throw new AssertionError("Failed to close the output stream", e);
    }
  }

  @Benchmark
  public void longExpression(Blackhole blackhole) throws IOException {
    try (var inlineCtx = inlineSource.inlineContextFactory().create()) {
      var tuppleOpt = compiler.runInline(longExpression, inlineCtx);
      if (tuppleOpt.isEmpty()) {
        throw new AssertionError("Unexpected: inline compilation should succeed");
      }
      blackhole.consume(tuppleOpt);
    }
  }
}
