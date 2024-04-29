package org.enso.compiler.benchmarks.module;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.CodeGenerator;
import org.enso.compiler.benchmarks.Utils;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.Type;
import org.graalvm.polyglot.Context;
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
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.infra.BenchmarkParams;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Measure compilation of a module with a lot of small methods with variable number of arguments.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ManySmallMethodsBenchmark {

  private static final int METHODS_CNT = 30;

  /** Maximum number of arguments of a single method. */
  private static final int MAX_ARGS = 10;

  /** Maximum number of local variables per method */
  private static final int MAX_LOCAL_VARS = 5;

  /** Maximum number of expressions per method, excluding new variable definitions. */
  private static final int MAX_METHOD_SIZE = 7;

  /** Maximum arity of an expression in method's body. */
  private static final int MAX_EXPR_SIZE = 5;

  private final Random random = new Random(42);
  private final StringBuilder sb = new StringBuilder();
  private Context context;
  private Compiler compiler;
  private Module module;
  private OutputStream out;

  @Setup
  public void setup(BenchmarkParams params) throws IOException {
    this.out = new ByteArrayOutputStream();
    this.context = Utils.createDefaultContextBuilder().logHandler(out).out(out).err(out).build();
    var ensoCtx = Utils.leakEnsoContext(context);
    List<Method> methods = new ArrayList<>();

    for (int methodIdx = 0; methodIdx < METHODS_CNT; methodIdx++) {
      methods.add(createMethod(methodIdx));
    }

    sb.append(System.lineSeparator());
    sb.append("main = ").append(System.lineSeparator());
    // Make sure that every method is called, with correct number of arguments
    var codeGen = new CodeGenerator();
    for (var method : methods) {
      sb.append("    ").append(method.name);
      for (int i = 0; i < method.argCount; i++) {
        sb.append(" ").append(codeGen.nextLiteral());
      }
      sb.append(System.lineSeparator());
    }

    var code = sb.toString();
    var srcFile = Utils.createSrcFile(code, "manySmallMethods.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = context.eval(src);
    var assocTypeValue = module.invokeMember(MethodNames.Module.GET_ASSOCIATED_TYPE);
    var assocType = (Type) Utils.unwrapReceiver(context, assocTypeValue);
    var moduleScope = assocType.getDefinitionScope();
    this.module = moduleScope.getModule();
    this.compiler = ensoCtx.getCompiler();
  }

  private Method createMethod(int methodIdx) {
    var methodName = "method_" + methodIdx;
    sb.append(methodName).append(" ");
    var argCount = random.nextInt(0, MAX_ARGS + 1);
    Set<String> args = new HashSet<>();
    for (int argIdx = 0; argIdx < argCount; argIdx++) {
      var argName = "arg_" + argIdx;
      sb.append(argName).append(" ");
      args.add(argName);
    }
    sb.append(" = ").append(System.lineSeparator());
    var codeGen = new CodeGenerator(args);
    var localVarsCnt = random.nextInt(0, MAX_LOCAL_VARS);
    for (int i = 0; i < localVarsCnt; i++) {
      sb.append("    ")
          .append(codeGen.defineNewVariable(MAX_EXPR_SIZE))
          .append(System.lineSeparator());
    }
    var methodSize = random.nextInt(0, MAX_METHOD_SIZE);
    for (int i = 0; i < methodSize; i++) {
      var exprSize = random.nextInt(0, MAX_EXPR_SIZE);
      var expr = codeGen.createExpressionFromDefinedIdentifiers(exprSize);
      sb.append("    ").append(expr).append(System.lineSeparator());
    }
    var lastExpr =
        codeGen.getUnusedIdentifiers().stream()
            .reduce((acc, ident) -> acc + " + " + ident)
            .orElse("42");
    sb.append("    ").append(lastExpr).append(System.lineSeparator());
    sb.append(System.lineSeparator());
    return new Method(methodName, argCount);
  }

  @TearDown
  public void tearDown() throws IOException {
    if (!out.toString().isEmpty()) {
      throw new AssertionError("Unexpected output from the compiler: " + out.toString());
    }
    out.close();
    context.close();
  }

  @Benchmark
  public void manySmallMethods(Blackhole blackhole) {
    var compilerResult = compiler.run(module.asCompilerModule());
    blackhole.consume(compilerResult);
  }

  private record Method(String name, int argCount) {}
}
