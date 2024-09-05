package org.enso.compiler.benchmarks.module;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.Compiler;
import org.enso.compiler.benchmarks.Utils;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.Type;
import org.enso.profiling.snapshot.HeapDumpSnapshot;
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
 * This benchmark imports all the symbols from libraries, that are normally imported in the IDE
 * template. This benchmark focuses on performance of import/export resolution compiler phase. The
 * IR cache is enabled, so that modules that are imported from standard libraries are not
 * re-compiled.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ImportStandardLibrariesBenchmark {

  /** Manually maintained list of symbols that are known to be imported. */
  private static final List<String> KNOWN_IMPORTED_SYMBOLS =
      List.of(
          // from Base
          "True",
          "False",
          "Vector",
          "Vector.build",
          // from Table
          "Table",
          "Column.from_vector",
          "Value_Type.Boolean",
          "Aggregate_Column.Count",
          "Excel_Format.read",
          "Previous_Value",
          "Join_Kind.Inner",
          // From Database
          "SQL_Query",
          "Postgres",
          "SQLite.connect",
          "Credentials.Username_And_Password",
          "Connection_Options.Connection_Options");

  private static final String IMPORTS =
      """
from Standard.Base import all
from Standard.Table import all
from Standard.Database import all
from Standard.Visualization import all
""";

  /**
   * Number of symbols that will be resolved. I.e., an expression will be created for them, so that
   * we are sure that they are imported and resolved.
   */
  private static final int RESOLVED_SYMBOLS_CNT = 20;

  private Context context;
  private Compiler compiler;
  private Module module;
  private OutputStream out;

  @Setup
  public void setup() throws IOException {
    this.out = new ByteArrayOutputStream();
    this.context =
        Utils.createDefaultContextBuilder()
            // Enable IR caches - we don't want to compile the imported modules from the standard
            // libraries
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .logHandler(out)
            .out(out)
            .err(out)
            .build();
    var ensoCtx = Utils.leakEnsoContext(context);

    Set<String> symbolsToUse =
        KNOWN_IMPORTED_SYMBOLS.stream().limit(RESOLVED_SYMBOLS_CNT).collect(Collectors.toSet());

    var sb = new StringBuilder();
    sb.append(IMPORTS);
    sb.append(System.lineSeparator());
    sb.append("main =").append(System.lineSeparator());

    for (var symbolToUse : symbolsToUse) {
      sb.append("    ").append(symbolToUse).append(System.lineSeparator());
    }

    var code = sb.toString();
    var srcFile = Utils.createSrcFile(code, "importStandardLibraries.enso");
    var src = Source.newBuilder(LanguageInfo.ID, srcFile).build();
    var module = context.eval(src);
    var assocTypeValue = module.invokeMember(MethodNames.Module.GET_ASSOCIATED_TYPE);
    var assocType = (Type) Utils.unwrapReceiver(context, assocTypeValue);
    var moduleScope = assocType.getDefinitionScope();
    this.module = moduleScope.getModule();
    this.compiler = ensoCtx.getCompiler();
  }

  @TearDown
  public void teardown() {
    if (!out.toString().isEmpty()) {
      throw new AssertionError("Unexpected output (errors?) from the compiler: " + out.toString());
    }
    context.close();
  }

  @Benchmark
  public void importStandardLibraries(Blackhole blackhole) {
    var compilerResult = compiler.run(module.asCompilerModule());
    blackhole.consume(compilerResult);
  }

  void run() throws IOException {
    setup();
    var snapshot = new HeapDumpSnapshot();
    try {
      compiler.run(module.asCompilerModule());
      snapshot.generateSnapshot(Paths.get("/tmp/compiler.hprof"));
    } finally {
      teardown();
    }
  }

  private static void main(String[] args) throws IOException {
    var benchmark = new ImportStandardLibrariesBenchmark();
    benchmark.run();
  }
}
