package org.enso.test.utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import org.enso.common.RuntimeOptions;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Context.Builder;
import org.graalvm.polyglot.Value;
import org.slf4j.LoggerFactory;

/** Utility methods for creating and running Enso projects. */
public class ProjectUtils {
  private ProjectUtils() {}

  /**
   * Creates temporary project directory structure with a given main source content. No need to
   * clean it up, as it is managed by JUnit TemporaryFolder rule. Note that we need to create a
   * project, otherwise the private stuff won't work.
   *
   * @param projName Name of the project (as defined in package.yaml).
   * @param mainSrc Main.enso source content
   * @param projDir Root directory of the project. Will be populated with the project structure.
   */
  public static void createProject(String projName, String mainSrc, Path projDir)
      throws IOException {
    var modules = Set.of(new SourceModule(QualifiedName.fromString("Main"), mainSrc));
    createProject(projName, modules, projDir);
  }

  /**
   * Creates a temporary project directory structure with all the given modules and their content.
   * Creates also the package descriptor. The created project directory structure is eligible for
   * running via {@code enso --run <projDir>}.
   *
   * @param projName Name of the project
   * @param modules Set of modules. If the main module is not present in the set, an exception will
   *     be thrown once you try to {@link #testProjectRun(Builder, Path, Consumer) test} the project
   *     run. Note that set of modules without a main module makes sense only if you intend to test
   *     the compilation and not running.
   * @param projDir A directory in which the whole project structure will be created. Must exist and
   *     be a directory.
   */
  public static void createProject(String projName, Set<SourceModule> modules, Path projDir)
      throws IOException {
    if (!projDir.toFile().exists() || !projDir.toFile().isDirectory()) {
      throw new IllegalArgumentException(
          "Project directory " + projDir + " must already be created");
    }
    var projYaml =
        """
name: %s
version: 0.0.1
prefer-local-libraries: true
        """.formatted(projName);
    var yamlPath = projDir.resolve("package.yaml");
    Files.writeString(yamlPath, projYaml);
    assert yamlPath.toFile().exists();
    var srcDir = Files.createDirectory(projDir.resolve("src"));
    assert srcDir.toFile().exists();
    for (var module : modules) {
      var relativePath = String.join(File.separator, module.name().pathAsJava());
      var modDirPath = srcDir.resolve(relativePath);
      Files.createDirectories(modDirPath);
      var modPath = modDirPath.resolve(module.name().item() + ".enso");
      Files.writeString(modPath, module.code());
    }
  }

  /**
   * Tests running the project located in the given {@code projDir}. Is equal to running {@code enso
   * --run <projDir>}.
   *
   * @param ctxBuilder A context builder that might be initialized with some specific options.
   * @param projDir Root directory of the project.
   * @param resultConsumer Any action that is to be evaluated on the result of running the {@code
   *     main} method
   */
  public static void testProjectRun(
      Context.Builder ctxBuilder, Path projDir, Consumer<Value> resultConsumer) {
    if (!(projDir.toFile().exists() && projDir.toFile().isDirectory())) {
      throw new IllegalArgumentException(
          "Project directory " + projDir + " must already be created");
    }
    try (var ctx =
        ctxBuilder
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
      if (!mainSrcPath.toFile().exists()) {
        throw new IllegalArgumentException("Main module not found in " + projDir);
      }
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocMainModType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
      var res = mainMethod.execute();
      resultConsumer.accept(res);
    }
  }

  /**
   * Just a wrapper for {@link ProjectUtils#testProjectRun(Builder, Path, Consumer)}.
   *
   * @param projDir Root directory of the project.
   * @param resultConsumer Any action that is to be evaluated on the result of running the {@code
   *     main} method
   */
  public static void testProjectRun(Path projDir, Consumer<Value> resultConsumer) {
    testProjectRun(ContextUtils.defaultContextBuilder(), projDir, resultConsumer);
  }

  /** Deletes provided directory recursively. */
  public static void deleteRecursively(Path rootDir) throws IOException {
    var deletedFiles =
        Files.walk(rootDir)
            .sorted(Comparator.reverseOrder())
            .map(Path::toFile)
            .map(f -> new FileDeletion(f.getPath(), f.delete()))
            .filter(d -> !d.deleted())
            .collect(Collectors.toList());
    if (rootDir.toFile().exists()) {
      var logger = LoggerFactory.getLogger(ProjectUtils.class);
      logger.error("{} root directory failed to delete because of the following path(s):", rootDir);
      deletedFiles.forEach(d -> logger.error(" - {}", d.filePath()));
    }
  }

  record FileDeletion(String filePath, boolean deleted) {}
}
