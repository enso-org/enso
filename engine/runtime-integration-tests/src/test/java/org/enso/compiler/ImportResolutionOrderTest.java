package org.enso.compiler;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.compiler.phase.ImportResolver;
import org.enso.interpreter.test.TestBase;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.PolyglotException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Tests the order in which modules are compiled when resolving imports. The contract is that all
 * the imported modules must be compiled BEFORE the main module (the one that imports those
 * modules). Tests {@link ImportResolver#mapImports(Module, boolean)} that should return a list of
 * modules to be compiled in a topological order.
 */
public class ImportResolutionOrderTest extends TestBase {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();
  private static final String mainModName = "local.Proj.Main";

  @Test
  public void importedModuleIsCompiledBeforeMainModule() {
    var importedMod =
        new SourceModule(QualifiedName.fromString("Mod"), """
            type T
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.Mod import T
            """);
    var projDir = createProject(Set.of(importedMod, mainMod));
    var modsToCompile = runImportResolution(projDir, mainModName);
    assertThat(modsToCompile.size(), is(2));
    assertThat(
        "Imported module must be the first to compile. modNamesToCompile = " + modsToCompile,
        modsToCompile,
        contains(Arrays.asList(containsString("Mod"), containsString("Main"))));
  }

  @Test
  public void twoImportedModulesAreCompiledBeforeMain() {
    var mod1 =
        new SourceModule(QualifiedName.fromString("Mod1"), """
            type T1
            """);
    var mod2 =
        new SourceModule(QualifiedName.fromString("Mod2"), """
            type T2
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.Mod1 import T1
            from project.Mod2 import T2
            """);
    var projDir = createProject(Set.of(mod1, mod2, mainMod));
    var modNamesToCompile = runImportResolution(projDir, mainModName);
    assertThat(modNamesToCompile.size(), is(3));
    assertThat(
        "Main module should be compile as the last one. modNamesToCompile = " + modNamesToCompile,
        modNamesToCompile,
        contains(
            Arrays.asList(
                anyOf(containsString("Mod1"), containsString("Mod2")),
                anyOf(containsString("Mod1"), containsString("Mod2")),
                containsString("Main"))));
  }

  @Test
  public void twoImportedModulesAreCompiledBeforeMainTransitivelly() {
    var mod1 =
        new SourceModule(QualifiedName.fromString("Mod1"), """
            type T1
            """);
    var mod2 =
        new SourceModule(
            QualifiedName.fromString("Mod2"),
            """
            from project.Mod1 import T1
            type T2
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.Mod2 import T2
            """);
    var projDir = createProject(Set.of(mod1, mod2, mainMod));
    var modNamesToCompile = runImportResolution(projDir, mainModName);
    assertThat(modNamesToCompile.size(), is(3));
    assertThat(
        "Main module should be compile as the last one. modNamesToCompile = " + modNamesToCompile,
        modNamesToCompile,
        contains(
            Arrays.asList(containsString("Mod1"), containsString("Mod2"), containsString("Main"))));
  }

  @Test
  public void allStdBaseModulesAreCompiledBeforeMain() {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from Standard.Base import all
            """);
    var projDir = createProject(Set.of(mainMod));
    var modsToCompile = runImportResolution(projDir, mainModName);
    assertThat(modsToCompile.size(), is(greaterThan(1)));
    var lastModToCompile = modsToCompile.get(modsToCompile.size() - 1);
    assertThat(
        "Main module should be compiled as the last one", lastModToCompile, containsString("Main"));
  }

  private Path createProject(Set<SourceModule> modules) {
    try {
      return createProject("Proj", modules, tempFolder);
    } catch (IOException e) {
      fail("Project creation failed: " + e.getMessage());
    }
    return null;
  }

  /**
   * Runs import resolution on the given project, starting from the given module name. Just wraps
   * the invocation of {@link ImportResolver#mapImports(Module, boolean)}.
   *
   * @param projDir Root directory of the project.
   * @param startModName FQN of a module from which the import resolution should start.
   * @return List of module names to compile in the topological order.
   */
  private static List<String> runImportResolution(Path projDir, String startModName) {
    assert projDir.toFile().exists() && projDir.toFile().isDirectory();
    var out = new ByteArrayOutputStream();
    List<Module> modulesToCompile = List.of();
    try (var ctx =
        defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .out(out)
            .err(out)
            .build()) {
      var ensoCtx = leakContext(ctx);
      var compiler = ensoCtx.getCompiler();
      var mainMod = compiler.getModule(startModName).get();
      var impResolver = new ImportResolver(compiler);
      var res = impResolver.mapImports(mainMod, false);
      modulesToCompile = ScalaConversions.asJava(res._1);
      var modsFromBindingsCache = ScalaConversions.asJava(res._2);
      assertThat("bindingsCaching is disabled", modsFromBindingsCache.isEmpty(), is(true));
    } catch (PolyglotException e) {
      fail("Import resolution should succeed. Instead got exception: " + e.getMessage());
    }
    return modulesToCompile.stream().map(m -> m.getName().toString()).toList();
  }
}
