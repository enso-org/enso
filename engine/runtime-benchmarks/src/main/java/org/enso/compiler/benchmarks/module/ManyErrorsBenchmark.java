package org.enso.compiler.benchmarks.module;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.CodeGenerator;
import org.enso.compiler.benchmarks.Utils;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.Type;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
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
 * Measure compilation of a module with one method that contains a lot of errors - syntactical
 * errors and unknown identifiers. The compiler should be able to recover from errors and so it
 * should compile the whole module and not stop after the first error.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ManyErrorsBenchmark {
  private static final int ERRORS_CNT = 30;
  private static final int IDENTIFIERS_CNT = 40;
  private static final int EXPR_SIZE = 5;
  private static final List<String> UNDEFINED_IDENTIFIERS =
      List.of("FOO_BAR", "Bazzzzz", "Type.Constructor.Foo.Bar.Baz");
  private Context context;
  private Compiler compiler;
  private Module module;
  private OutputStream out;

  private final Random random = new Random(42);

  @Setup
  public void setup(BenchmarkParams params) throws IOException {
    this.out = new ByteArrayOutputStream();
    this.context =
        Utils.createDefaultContextBuilder()
            .option(RuntimeOptions.STRICT_ERRORS, "false")
            .logHandler(out)
            .out(out)
            .err(out)
            .build();
    var ensoCtx = Utils.leakEnsoContext(context);
    var sb = new StringBuilder();
    var codeGen = new CodeGenerator();
    var definedIdentifiers = codeGen.createIdentifiers(IDENTIFIERS_CNT);
    sb.append("main = ").append(System.lineSeparator());
    for (String ident : definedIdentifiers) {
      sb.append("    ")
          .append(ident)
          .append(" = ")
          .append(codeGen.nextLiteral())
          .append(System.lineSeparator());
    }
    for (int i = 0; i < ERRORS_CNT; i++) {
      var rndInt = random.nextInt(0, 3);
      switch (rndInt) {
          // Expression with unknown identifiers
        case 0 -> {
          var expr = codeGen.createExpression(UNDEFINED_IDENTIFIERS, EXPR_SIZE);
          sb.append("    ").append(expr).append(System.lineSeparator());
        }
          // Inline type ascription with unknown identifier
        case 1 -> {
          var expr = codeGen.createExpression(definedIdentifiers, EXPR_SIZE);
          sb.append("    ")
              .append("    var : (Type.Constructor.Foo Bar.Baz A B) = ")
              .append(expr)
              .append(System.lineSeparator());
        }
          // Put arrows before, after, and between expressions
        case 2 -> {
          var expr1 = codeGen.createExpression(definedIdentifiers, EXPR_SIZE);
          var expr2 = codeGen.createExpression(definedIdentifiers, EXPR_SIZE);
          sb.append("    ")
              .append(" -> ")
              .append(expr1)
              .append(" -> ")
              .append(expr2)
              .append(" -> ")
              .append(System.lineSeparator());
        }
        default -> throw new AssertionError("Unexpected random integer: " + rndInt);
      }
    }
    var code = sb.toString();
    var srcFile = Utils.createSrcFile(code, "manyErrorsBenchmark.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = context.eval(src);
    var assocTypeValue = module.invokeMember(MethodNames.Module.GET_ASSOCIATED_TYPE);
    var assocType = (Type) Utils.unwrapReceiver(context, assocTypeValue);
    var moduleScope = assocType.getDefinitionScope();
    this.module = moduleScope.getModule();
    this.compiler = ensoCtx.getCompiler();
  }

  @TearDown
  public void tearDown() throws IOException {
    // Teardown is called at the end of all the iterations. This means that the number of errors
    // reported into `out` should be much bigger than `ERRORS_CNT`. Let's iterate over some lines
    // and see if this is true.
    var errorsFound = 0;
    for (String line : out.toString().split(System.lineSeparator())) {
      if (errorsFound == ERRORS_CNT) {
        break;
      }
      if (line.matches(".*: error: .*")) {
        errorsFound++;
      }
    }
    if (errorsFound != ERRORS_CNT) {
      throw new AssertionError("Expected " + ERRORS_CNT + " errors, but found " + errorsFound);
    }
    out.close();
    context.close();
  }

  @Benchmark
  public void manyErrors(Blackhole blackhole) {
    var compilerResult = compiler.run(module.asCompilerModule());
    blackhole.consume(compilerResult);
  }
}
