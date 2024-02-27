package org.enso.compiler.benchmarks.inline;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.CodeGenerator;
import org.enso.compiler.benchmarks.Utils;
import org.enso.compiler.context.InlineContext;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.MethodNames.TopScope;
import org.enso.polyglot.RuntimeOptions;
import org.enso.polyglot.runtime.Runtime;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
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
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Measures the inline compilation, that is the compilation that is requested inside a method.
 * Simulates a scenario where there is an existing method and we are trying to insert a new
 * expression into it.
 * <p>
 * There is just a single `main` method that contains {@link #LOCAL_VARS_CNT} local variables.
 * One benchmark measures the time it takes to inline compile a long expression that uses all the
 * local variables. The other benchmark measures the time it takes to inline compile an expression
 * that contains some undefined identifiers and thus, should fail to compile.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(0)
@Warmup(iterations = 1)
@Measurement(iterations = 1)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class InlineCompilerBenchmarks {
  private static final int LONG_EXPR_SIZE = 5;

  private final OutputStream out = new ByteArrayOutputStream();
  /**
   * How many variables should be declared in the main method.
   */
  private static final int LOCAL_VARS_CNT = 40;
  private Compiler compiler;
  private InlineContext mainInlineContext;
  private String longExpression;
  private String expressionWithErrors;

  @Setup
  public void setup() throws IOException {
    var ctx = Utils.createDefaultContextBuilder()
        .out(out)
        .err(out)
        .logHandler(out)
        .option(RuntimeOptions.STRICT_ERRORS, "false")
        .build();

    var sb = new StringBuilder();
    sb.append("main = ")
        .append(System.lineSeparator());
    var codeGen = new CodeGenerator();
    Set<String> localVarNames = new HashSet<>();
    for (int i = 0; i < LOCAL_VARS_CNT; i++) {
      var varName = "loc_var_" + i;
      localVarNames.add(varName);
      var literal = codeGen.nextLiteral();
      sb.append("    ")
          .append(varName)
          .append(" = ")
          .append(literal)
          .append(System.lineSeparator());
    }
    // In the last expression of main method, use all the variables, so that there is no unused
    // variable warning
    var lastExpr = localVarNames.stream()
        .reduce((acc, varName) -> acc + " + " + varName)
        .orElseThrow();
    sb.append("    ")
        .append(lastExpr)
        .append(System.lineSeparator());


    var ensoCtx = Utils.leakEnsoContext(ctx);
    var srcFile = Utils.createSrcFile(sb.toString(), "inlineBenchmark.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = ctx.eval(src);
    var moduleAssocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var assocTypeReceiver = (Type) Utils.unwrapReceiver(ctx, moduleAssocType);
    var moduleScope = assocTypeReceiver.getDefinitionScope();
    var mainFunc = moduleScope.getMethodForType(assocTypeReceiver, "main");
    var mainFuncRootNode = (MethodRootNode) mainFunc.getCallTarget().getRootNode();
    var mainLocalScope = mainFuncRootNode.getLocalScope();
    compiler = ensoCtx.getCompiler();
    mainInlineContext = InlineContext.fromJava(
        mainLocalScope,
        moduleScope.getModule().asCompilerModule(),
        scala.Option.apply(false),
        ensoCtx.getCompilerConfig(),
        scala.Option.apply(compiler.packageRepository()));
    longExpression = createLongExpression(localVarNames);
    expressionWithErrors = "UNDEFINED * " + longExpression + " * UNDEFINED";
  }

  private String createLongExpression(Set<String> localVars) {
    var codeGen = new CodeGenerator(localVars);
    return codeGen.createExpressionFromDefinedIdentifiers(LONG_EXPR_SIZE);
  }

  @TearDown
  public void tearDown() {
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
  public void longExpression(Blackhole blackhole) {
    var tuppleOpt = compiler.runInline(longExpression, mainInlineContext);
    if (tuppleOpt.isEmpty()) {
      throw new AssertionError("Unexpected: inline compilation should succeed");
    }
    blackhole.consume(tuppleOpt);
  }

  @Benchmark
  public void expressionWithErrors() {
    var tuppleOpt = compiler.runInline(expressionWithErrors, mainInlineContext);
    if (tuppleOpt.isDefined()) {
      throw new AssertionError("Unexpected: inline compilation should fail");
    }
  }

}
