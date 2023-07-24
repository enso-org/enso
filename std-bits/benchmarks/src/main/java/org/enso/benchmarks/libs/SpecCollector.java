package org.enso.benchmarks.libs;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.benchmarks.BenchSuite;
import org.enso.polyglot.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;

/**
 * Collect benchmark specifications from source files.
 */
public class SpecCollector {
  private final File rootDir;
  private final Context ctx;
  private static final String allSuitesVarName = "all";

  public SpecCollector(File benchProjectDir, File languageHomeOverride) {
    if (!benchProjectDir.exists() || !benchProjectDir.isDirectory()) {
      throw new IllegalArgumentException(benchProjectDir + " is not a directory.");
    }
    this.rootDir = benchProjectDir;
    this.ctx = Context.newBuilder()
        .allowExperimentalOptions(true)
        .allowIO(IOAccess.ALL)
        .allowAllAccess(true)
        .logHandler(new ByteArrayOutputStream())
        .option(
            "enso.projectRoot",
            benchProjectDir.getAbsolutePath()
        )
        .option(
            "enso.languageHomeOverride",
            languageHomeOverride.getAbsolutePath()
        )
        .build();
  }

  public Collection<BenchSuite> collectAllBenchSpecs() {
    try (Stream<Path> stream = Files.walk(rootDir.toPath())) {
      return stream
          .map(Path::toFile)
          .filter(file -> file.isFile() && file.canRead() && file.getName().endsWith(".enso"))
          .map(this::collectBenchSpecsFromSingleFile)
          .filter(Objects::nonNull)
          .collect(Collectors.toList());
    } catch (IOException e) {
      throw new IllegalStateException("Unreachable", e);
    }

  }

  /**
   * Collects benchmark specifications from a single file. Returns null if no specification
   * is found in the file.
   * @param benchFile Path to the source file.
   * @return null if there are no benchmark specs in the file.
   */
  private BenchSuite collectBenchSpecsFromSingleFile(File benchFile) {
    Source source;
    try {
      source = Source.newBuilder("enso", benchFile).build();
    } catch (IOException e) {
      throw new IllegalStateException("Unreachable", e);
    }
    Value module = ctx.eval(source);
    Value moduleType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    Value allSuitesVar = module.invokeMember(Module.GET_METHOD, moduleType, allSuitesVarName);
    if (!allSuitesVar.isNull()) {
      try {
        return module
            .invokeMember(Module.EVAL_EXPRESSION, "all")
            .as(BenchSuite.class);
      } catch (PolyglotException e) {
        // TODO: Replace with proper logging
        System.err.println("WARN: An exception occured while evaluating benchmark suite var: " + e.getMessage());
      }
    }
    return null;
  }
}
