package org.enso.benchmarks.processor;

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
import org.enso.benchmarks.ModuleBenchSuite;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.MethodNames.TopScope;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;

/**
 * Collect benchmark specifications from source files.
 */
public class SpecCollector {

  private final File rootDir;
  private final Context ctx;
  private static final String allSuitesVarName = "all";
  private final String benchPackagePrefix = "local.Benchmarks";
  private final String ensoSuffix = ".enso";

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

  public ModuleBenchSuite collectBenchSpecFromModuleName(String moduleName) {
    Objects.requireNonNull(moduleName);
    if (!moduleName.startsWith(benchPackagePrefix)) {
      throw new IllegalArgumentException("Module name must start with " + benchPackagePrefix);
    }
    Value module = ctx.getBindings(LanguageInfo.ID).invokeMember(TopScope.GET_MODULE, moduleName);
    return collectBenchSpecFromModule(module, moduleName);
  }

  Collection<ModuleBenchSuite> collectAllBenchSpecs() {
    try (Stream<Path> stream = Files.walk(rootDir.toPath())) {
      return stream
          .map(Path::toFile)
          .filter(file -> file.isFile() && file.canRead() && file.getName().endsWith(ensoSuffix))
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
  private ModuleBenchSuite collectBenchSpecsFromSingleFile(File benchFile) {
    Path relativePath = rootDir.toPath().relativize(benchFile.toPath());
    // Strip the "src" directory
    Path modulePath = relativePath.subpath(1, relativePath.getNameCount());
    String moduleFullName = benchPackagePrefix + "." + modulePath.toString().replace(File.separatorChar, '.');
    moduleFullName = moduleFullName.substring(0, moduleFullName.length() - ensoSuffix.length());
    Value module = ctx.getBindings(LanguageInfo.ID).invokeMember(TopScope.GET_MODULE, moduleFullName);
    return collectBenchSpecFromModule(module, moduleFullName);
  }

  private ModuleBenchSuite collectBenchSpecFromModule(Value module, String moduleQualifiedName) {
    Value moduleType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    Value allSuitesVar = module.invokeMember(Module.GET_METHOD, moduleType, allSuitesVarName);
    if (!allSuitesVar.isNull()) {
      BenchSuite suite;
      try {
        suite = module
            .invokeMember(Module.EVAL_EXPRESSION, "all")
            .as(BenchSuite.class);
        return new ModuleBenchSuite(suite, moduleQualifiedName);
      } catch (PolyglotException e) {
        // TODO: Replace with proper logging
        System.err.println("WARN: An exception occured while evaluating benchmark suite var: " + e.getMessage());
        return null;
      }
    }
    return null;
  }
}
