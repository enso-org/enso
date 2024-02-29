package org.enso.interpreter.bench.benchmarks.semantic;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.logging.Level;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.openjdk.jmh.infra.BenchmarkParams;

final class SrcUtil {
  private SrcUtil() {}

  static String findName(BenchmarkParams params) {
    return params.getBenchmark().replaceFirst(".*\\.", "");
  }

  static Source source(String benchmarkName, String code) throws IOException {
    var d = new File(new File(new File("."), "target"), "bench-data");
    d.mkdirs();
    var f = new File(d, benchmarkName + ".enso");
    try (var w = new FileWriter(f)) {
      w.write(code);
    }
    return Source.newBuilder("enso", f).build();
  }

  static Source read(String benchmarkName) throws IOException {
    String resource = benchmarkName + ".enso";
    var url = SrcUtil.class.getResource(resource);
    Objects.requireNonNull(url, "Searching for " + resource);
    return Source.newBuilder("enso", url).name(resource).build();
  }

  static Value getMainMethod(Context context, String benchmarkName, String code)
      throws IOException {
    var src = source(benchmarkName, code);
    var module = context.eval(src);
    var moduleType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var main = module.invokeMember(Module.GET_METHOD, moduleType, "main");
    if (!main.canExecute()) {
      throw new AssertionError("Main method should be executable");
    }
    return main;
  }

  /**
   * Typical builder suitable for benchmarking.
   *
   * @return preconfigured builder to use as a base for benchmarking
   */
  static Context.Builder newContextBuilder() {
    return Context.newBuilder()
        .allowExperimentalOptions(true)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
        .logHandler(System.err)
        .allowIO(IOAccess.ALL)
        .allowAllAccess(true)
        .option("engine.MultiTier", "false")
        .option("engine.BackgroundCompilation", "false")
        .option("engine.CompilationFailureAction", "Print")
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath());
  }
}
