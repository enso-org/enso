package org.enso.compiler.benchmarks.module;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
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

/**
 * Measure compilation of a module with a single long method with a format like:
 * <pre>
 * main =
 *    obj1 = ...
 *    obj2 = ...
 *    obj3 = ...
 * </pre>
 * This is the format that is used by the IDE.
 * This should measure mostly the performance of the dataflow analysis pass.
 */
@BenchmarkMode(Mode.AverageTime)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ManyLocalVarsBenchmark {

  /**
   * Total count of local variables in the `main` method. Every variable is defined on
   * a new line.
   */
  private static final int IDENTIFIERS_CNT = 10;
  private Context context;
  private Compiler compiler;
  private Module module;
  private OutputStream out;

  @Setup
  public void setup(BenchmarkParams params) throws IOException {
    this.out = new ByteArrayOutputStream();
    this.context = Utils
        .createDefaultContextBuilder()
        .logHandler(out)
        .out(out)
        .err(out)
        .build();
    var ensoCtx = Utils.leakEnsoContext(context);
    var sb = new StringBuilder();
    var codeGen = new CodeGenerator();
    var allIdentifiers = codeGen.createIdentifiers(IDENTIFIERS_CNT);
    var firstIdent = allIdentifiers.get(0);
    List<String> initializedIdentifiers = new ArrayList<>();
    initializedIdentifiers.add(firstIdent);
    sb.append("main = ").append(System.lineSeparator());
    sb.append("    ")
        .append(firstIdent)
        .append(" = ")
        .append("42")
        .append(System.lineSeparator());

    Set<String> usedIdentifiers = new HashSet<>();
    allIdentifiers
        .stream()
        .skip(1)
        .forEach(
            identifier -> {
              var maxExprSize = Math.min(5, initializedIdentifiers.size() - 1);
              sb.append("    ")
                  .append(identifier)
                  .append(" = ")
                  .append(codeGen.createExpression(initializedIdentifiers, usedIdentifiers, maxExprSize))
                  .append(System.lineSeparator());
              initializedIdentifiers.add(identifier);
            });

    assert initializedIdentifiers.size() == IDENTIFIERS_CNT;
    assert usedIdentifiers.size() <= IDENTIFIERS_CNT;
    if (usedIdentifiers.size() < IDENTIFIERS_CNT) {
      // Add a final line that uses the rest of the identifiers, so that there is no "Unused binding"
      // warning.
      List<String> unusedIdentifiers = new ArrayList<>(allIdentifiers);
      unusedIdentifiers.removeAll(usedIdentifiers);
      sb.append("    ")
          .append("result = ");
      sb.append(
        unusedIdentifiers
            .stream()
            .reduce((a, b) -> a + " + " + b)
            .orElseThrow()
      );
      sb.append(System.lineSeparator());
      // result is the return value from the main method
      sb.append("    ")
          .append("result")
          .append(System.lineSeparator());
    }
    var code = sb.toString();
    var srcFile = Utils.createSrcFile(code, "longMethodWithLotOfLocalVars.enso");
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
    if (!out.toString().isEmpty()) {
      throw new AssertionError("Unexpected output from the compiler: " + out.toString());
    }
    out.close();
    context.close();
  }

  @Benchmark
  public void longMethodWithLotOfLocalVars(Blackhole blackhole) {
    var compilerResult = compiler.run(module.asCompilerModule());
    if (compilerResult.compiledModules().size() != 1) {
      throw new AssertionError("Module compilation failed");
    }
  }
}
