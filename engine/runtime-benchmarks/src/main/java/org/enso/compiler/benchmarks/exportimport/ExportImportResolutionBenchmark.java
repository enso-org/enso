package org.enso.compiler.benchmarks.exportimport;

import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import org.enso.common.CompilationStage;
import org.enso.compiler.benchmarks.Utils;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.phase.ImportResolver;
import org.enso.compiler.phase.exports.ExportCycleException;
import org.enso.compiler.phase.exports.ExportsResolution;
import org.enso.interpreter.runtime.Module;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
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
import scala.jdk.javaapi.CollectionConverters;

/**
 * Benchmarks that measure performance of only {@link ImportResolver} and {@link ExportsResolution}.
 */
@BenchmarkMode(Mode.AverageTime)
@Fork(1)
@Warmup(iterations = 6)
@Measurement(iterations = 4)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
public class ExportImportResolutionBenchmark {
  private Path projDir;
  private ContextUtils ctx;
  private ImportResolver importResolver;
  private ExportsResolution exportsResolution;
  private CompilerContext.Module mainModule;
  private scala.collection.immutable.List<CompilerContext.Module> modulesToExportResolution;
  private scala.collection.immutable.List<CompilerContext.Module> resolvedModules;

  @Setup
  public void setup(BenchmarkParams params) throws IOException {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from Standard.Base import all
            """);
    this.projDir = Files.createTempDirectory("export-import-resolution-benchmark");
    ProjectUtils.createProject("Proj", Set.of(mainMod), projDir);
    // Create temp proj dir
    this.ctx = Utils.createDefaultContextBuilder().withProjectRoot(projDir).build();
    var ensoCtx = ctx.ensoContext();
    this.mainModule = ensoCtx.getPackageRepository().getLoadedModule("local.Proj.Main").get();
    var mainRuntimeMod = Module.fromCompilerModule(mainModule);
    assertTrue("main module should not yet be compiled", mainRuntimeMod.needsCompilation());
    assertThat(
        "main module should not yet be compiled",
        mainModule.getCompilationStage().equals(CompilationStage.INITIAL));
    this.importResolver = new ImportResolver(ensoCtx.getCompiler());
    this.exportsResolution = new ExportsResolution(ensoCtx.getCompiler().context());
  }

  @TearDown
  public void teardown(BenchmarkParams params) throws IOException {
    if (!isOutputEmpty()) {
      throw new AssertionError("Unexpected output (errors?) from the compiler: " + ctx.getOut());
    }
    ProjectUtils.deleteRecursively(projDir);
    ctx.close();

    // It is expected that there are more than 20 modules in Standard.Base, and all of them
    // should have been processed. Note that 20 is just a magic constant for sanity check.
    assertThat(modulesToExportResolution.size() > 20);
    var mods = CollectionConverters.asJava(modulesToExportResolution);
    for (var mod : mods) {
      var isImportResolved =
          mod.getCompilationStage().isAtLeast(CompilationStage.AFTER_IMPORT_RESOLUTION);
      assertThat(
          "Module '" + mod.getName() + "' is not resolved after import resolution",
          isImportResolved);
    }

    var benchName = params.getBenchmark();
    if (benchName.contains("runImportExportResolution")) {
      assertThat(resolvedModules.size() > 20);
      for (var mod : CollectionConverters.asJava(resolvedModules)) {
        var isExportResolved =
            mod.getCompilationStage().isAtLeast(CompilationStage.AFTER_IMPORT_RESOLUTION);
        assertThat(
            "Module '" + mod.getName() + "' is not resolved after export resolution",
            isExportResolved);
      }
    }
  }

  @Benchmark
  public void importsResolution(Blackhole blackhole) {
    var res = importResolver.mapImports(mainModule, false);
    modulesToExportResolution = res._1;
    blackhole.consume(res);
  }

  /**
   * {@link ExportsResolution export resolver} needs to be run after the imports resolution, so in
   * this benchmark, we run both import resolution and export resolution. In the other benchmark,
   * only import resolution is run.
   */
  @Benchmark
  public void importsAndExportsResolution(Blackhole blackhole) throws ExportCycleException {
    var res = importResolver.mapImports(mainModule, false);
    modulesToExportResolution = res._1;
    resolvedModules = exportsResolution.run(modulesToExportResolution);
    blackhole.consume(resolvedModules);
  }

  /**
   * These utility methods are used because simple {@code assert} statement might not work -
   * benchmarks are usually run without assertions enabled. This just makes sure that the given
   * condition is checked and not ignored.
   */
  private static void assertThat(boolean condition) {
    assertThat("", condition);
  }

  private static void assertThat(String msg, boolean condition) {
    if (!condition) {
      throw new AssertionError(msg);
    }
  }

  private boolean isOutputEmpty() {
    Predicate<String> isIgnored = (str) -> str.contains("in a different working directory");
    var linesCnt = ctx.getOut().lines().filter(line -> !isIgnored.test(line)).count();
    return linesCnt == 0;
  }
}
