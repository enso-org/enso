package org.enso.compiler.benchmarks.inline;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.Utils;
import org.enso.test.utils.ContextUtils;
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
 * <p>The expression inserted into the method contains some undefined identifiers and thus, should
 * fail to compile.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class InlineCompilerErrorBenchmark {
  /** How many variables should be declared in the main method. */
  private static final int LOCAL_VARS_CNT = 40;

  private static final int LONG_EXPR_SIZE = 5;

  /** How many variables should be declared in the main method. */
  private Compiler compiler;

  private ContextUtils ctx;
  private InlineContextResourceFactory mainInlineContextResourceFactory;
  private String expressionWithErrors;

  @Setup
  public void setup() throws IOException {
    ctx =
        Utils.createDefaultContextBuilder()
            .withModifiedContext(bldr -> bldr.option(RuntimeOptions.STRICT_ERRORS, "false"))
            .build();
    var ensoCtx = ctx.ensoContext();
    compiler = ensoCtx.getCompiler();

    var localVarNames = InlineContextUtils.localVarNames(LOCAL_VARS_CNT);
    var inlineSource = InlineContextUtils.createMainMethodWithLocalVars(ctx, localVarNames);
    var longExpression =
        InlineContextUtils.createLongExpression(inlineSource.localVarNames(), LONG_EXPR_SIZE);
    expressionWithErrors = "UNDEFINED * " + longExpression + " * UNDEFINED";
    mainInlineContextResourceFactory = inlineSource.inlineContextFactory();
  }

  @TearDown
  public void teardown() {
    if (ctx.getOut().isEmpty()) {
      throw new AssertionError("Expected some output (some errors) from the compiler");
    }
    ctx.close();
  }

  @Benchmark
  public void expressionWithErrors(Blackhole blackhole) throws IOException {
    try (var inlineCtx = mainInlineContextResourceFactory.create()) {
      var tuppleOpt = compiler.runInline(expressionWithErrors, inlineCtx);
      blackhole.consume(tuppleOpt);
    }
  }
}
