package org.enso.compiler.benchmarks.module;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.CodeGenerator;
import org.enso.compiler.benchmarks.Utils;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.Type;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
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

/** Measure compilation of a module with one method that contains a lot of nested blocks. */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ManyNestedBlocksBenchmark {

  /** Maximum nesting. */
  private static final int NESTED_BLOCKS_CNT = 40;

  /**
   * Number of cases in a case expression. Only the last case contains nested blocks, the rest are
   * simple literals.
   */
  private static final int CASES_CNT = 5;

  /** Number of lines per block. */
  private static final int BLOCK_SIZE = 5;

  /** How many new identifiers will be defined in each block. */
  private static final int IDENTIFIERS_CNT = 2;

  /** Maximum size of an expression on a single line. */
  private static final int MAX_EXPR_SIZE = 5;

  /** Maximum number of arguments of an anonymous function */
  private final int ANONYMOUS_FUNC_MAX_ARGS = 5;

  private final Random random = new Random(42);
  private final CodeGenerator codeGen = new CodeGenerator();
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
    sb.append("main = ").append(System.lineSeparator());
    createNestedBlocks(1);

    assert codeGen.getUnusedIdentifiers().isEmpty()
        : "All variables should be used in simple blocks";

    var code = sb.toString();
    var srcFile = Utils.createSrcFile(code, "manyNestedBlocks.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = context.eval(src);
    var assocTypeValue = module.invokeMember(MethodNames.Module.GET_ASSOCIATED_TYPE);
    var assocType = (Type) Utils.unwrapReceiver(context, assocTypeValue);
    var moduleScope = assocType.getDefinitionScope();
    this.module = moduleScope.getModule();
    this.compiler = ensoCtx.getCompiler();
  }

  /**
   * All the unused variables are used in the last expression in the block
   *
   * @param nestedBlockIdx Nesting index.
   */
  private void createSimpleBlock(int nestedBlockIdx) {
    var spaces = "    ".repeat(nestedBlockIdx);
    var exprsCnt = random.nextInt(1, BLOCK_SIZE);
    var exprSize = random.nextInt(0, MAX_EXPR_SIZE);
    for (int i = 0; i < IDENTIFIERS_CNT; i++) {
      var newVar = codeGen.defineNewVariable();
      sb.append(spaces)
          .append(newVar)
          .append(" = ")
          .append(codeGen.nextLiteral())
          .append(System.lineSeparator());
    }
    for (int i = 0; i < exprsCnt; i++) {
      var expr = codeGen.createExpressionFromDefinedIdentifiers(exprSize);
      sb.append(spaces).append(expr).append(System.lineSeparator());
    }
    Set<String> unusedIdentifiers = codeGen.getUnusedIdentifiers();
    var lastExpr =
        unusedIdentifiers.stream().reduce((acc, ident) -> acc + " + " + ident).orElse("42");
    sb.append(spaces).append(lastExpr).append(System.lineSeparator());
    unusedIdentifiers.forEach(codeGen::markVariableAsUsed);
  }

  private void createNestedBlocks(int nestedBlockIdx) {
    if (nestedBlockIdx >= NESTED_BLOCKS_CNT) {
      return;
    }
    createSimpleBlock(nestedBlockIdx);
    // Introduce a new nested block at the end of the current block
    if (nestedBlockIdx < NESTED_BLOCKS_CNT - 1) {
      var rndInt = random.nextInt(0, 4);
      var spaces = "    ".repeat(nestedBlockIdx);
      switch (rndInt) {
        case 0 -> {
          var ident = codeGen.chooseDefinedIdentifier();
          sb.append(spaces)
              .append("case ")
              .append(ident)
              .append(" of")
              .append(System.lineSeparator());
          // Only the last case expression contains nested blocks
          for (int i = 0; i < CASES_CNT - 1; i++) {
            sb.append(spaces)
                .append("    ")
                .append(codeGen.nextLiteral())
                .append(" -> ")
                .append(codeGen.nextLiteral())
                .append(System.lineSeparator());
          }
          sb.append(spaces)
              .append("    ")
              .append(codeGen.nextLiteral())
              .append(" -> ")
              .append(System.lineSeparator());
          createNestedBlocks(nestedBlockIdx + 2);
        }
        case 1 -> {
          var ident = codeGen.chooseDefinedIdentifier();
          sb.append(spaces)
              .append("if ")
              .append(ident)
              .append(" == 42 then 42 else")
              .append(System.lineSeparator());
          createNestedBlocks(nestedBlockIdx + 1);
        }
        case 2 -> {
          var ident = codeGen.chooseDefinedIdentifier();
          sb.append(spaces)
              .append("if ")
              .append(ident)
              .append(" == 42 then")
              .append(System.lineSeparator());
          createNestedBlocks(nestedBlockIdx + 1);
        }
          // Create a nested anonymous function
        case 3 -> {
          var argsCnt = random.nextInt(1, ANONYMOUS_FUNC_MAX_ARGS);
          sb.append(spaces);
          for (int i = 0; i < argsCnt; i++) {
            var newIdent = codeGen.defineNewVariable();
            sb.append(newIdent).append("-> ");
          }
          sb.append(System.lineSeparator());
          createNestedBlocks(nestedBlockIdx + 1);
        }
        default -> throw new IllegalStateException("Unexpected random int: " + rndInt);
      }
    }
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
  public void manyNestedBlocks(Blackhole blackhole) {
    var compilerResult = compiler.run(module.asCompilerModule());
    blackhole.consume(compilerResult);
  }
}
