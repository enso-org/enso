package org.enso.test.utils;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Set;
import java.util.function.Consumer;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Context.Builder;
import org.graalvm.polyglot.Value;

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
   * @param modules Set of modules. Must contain `Main` module.
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
    boolean mainModuleFound = false;
    for (var module : modules) {
      var relativePath = String.join(File.pathSeparator, module.name().pathAsJava());
      var modDirPath = srcDir.resolve(relativePath);
      Files.createDirectories(modDirPath);
      var modPath = modDirPath.resolve(module.name().item() + ".enso");
      Files.writeString(modPath, module.code());
      if (module.name().equals(QualifiedName.fromString("Main"))) {
        mainModuleFound = true;
      }
    }
    assert mainModuleFound;
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
    assert projDir.toFile().exists() && projDir.toFile().isDirectory();
    try (var ctx =
        ctxBuilder
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocMainModType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
      var res = mainMethod.execute();
      resultConsumer.accept(res);
    }
  }

  /**
   * Just a wrapper for {@link ContextUtils#testProjectRun(Builder, Path, Consumer)}.
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
    Files.walkFileTree(
        rootDir,
        new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
              throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
          }
        });
  }
}
