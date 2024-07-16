package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.contains;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import org.enso.compiler.phase.exports.ExportsResolution;
import org.enso.interpreter.runtime.Module;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ModuleUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Context;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Tests ordering of modules from {@link
 * org.enso.compiler.phase.exports.ExportsResolution#runSort(scala.collection.immutable.List)}. Some
 * tests are already in {@link org.enso.compiler.test.semantic.ImportExportTest}, but there are some
 * limitations, like no ability to create (and test) ordering of synthetic modules.
 */
public class ExportResolutionOrderingTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void testOrderingWithSubmoduleOfSyntheticModule() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("Synthetic_Module.A_Module"),
            """
        type A_Type
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.Synthetic_Module.A_Module.A_Type
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);

    try (var ctx = createContext(projDir)) {
      compile(ctx);
      var mainRuntimeMod = getLoadedModule(ctx, "local.Proj.Main");
      var aRuntimeMod = getLoadedModule(ctx, "local.Proj.Synthetic_Module.A_Module");
      var syntheticRuntimeMod = getLoadedModule(ctx, "local.Proj.Synthetic_Module");
      var sortedModules =
          runExportsResolutionSort(List.of(mainRuntimeMod, aRuntimeMod, syntheticRuntimeMod), ctx);
      assertThat(
          "Export relations should be: mainMod --> syntheticMod --> aMod",
          sortedModules,
          contains(aRuntimeMod, syntheticRuntimeMod, mainRuntimeMod));
    }
  }

  @Test
  public void testOrderingWithTwoSubmodulesOfSyntheticModule() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("Synthetic_Module.A_Module"),
            """
        type A_Type
        """);
    var bMod =
        new SourceModule(
            QualifiedName.fromString("Synthetic_Module.B_Module"),
            """
        type B_Type
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.Synthetic_Module.A_Module.A_Type
        export project.Synthetic_Module.B_Module.B_Type
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, bMod, mainMod), projDir);

    try (var ctx = createContext(projDir)) {
      compile(ctx);
      var mainRuntimeMod = getLoadedModule(ctx, "local.Proj.Main");
      var aRuntimeMod = getLoadedModule(ctx, "local.Proj.Synthetic_Module.A_Module");
      var bRuntimeMod = getLoadedModule(ctx, "local.Proj.Synthetic_Module.B_Module");
      var syntheticRuntimeMod = getLoadedModule(ctx, "local.Proj.Synthetic_Module");
      var sortedModules =
          runExportsResolutionSort(
              List.of(mainRuntimeMod, aRuntimeMod, bRuntimeMod, syntheticRuntimeMod), ctx);
      assertThat(
          "Export relations should be: mainMod --> syntheticMod --> aMod; mainMod --> syntheticMod"
              + " --> bMod",
          sortedModules,
          anyOf(
              contains(bRuntimeMod, aRuntimeMod, syntheticRuntimeMod, mainRuntimeMod),
              contains(aRuntimeMod, bRuntimeMod, syntheticRuntimeMod, mainRuntimeMod)));
    }
  }

  // TODO: Tracked by https://github.com/enso-org/enso/issues/10505
  @Ignore
  @Test
  public void testOrderingWithTwoSyntheticModules() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("Syn_1.Syn_2.A_Module"), """
        type A_Type
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.Syn_1.Syn_2.A_Module.A_Type
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);

    try (var ctx = createContext(projDir)) {
      compile(ctx);
      var mainRuntimeMod = getLoadedModule(ctx, "local.Proj.Main");
      var aRuntimeMod = getLoadedModule(ctx, "local.Proj.Syn_1.Syn_2.A_Module");
      var syn1RuntimeMod = getLoadedModule(ctx, "local.Proj.Syn_1");
      var syn2RuntimeMod = getLoadedModule(ctx, "local.Proj.Syn_1.Syn_2");
      var sortedModules =
          runExportsResolutionSort(
              List.of(mainRuntimeMod, aRuntimeMod, syn1RuntimeMod, syn2RuntimeMod), ctx);
      var sortedModNames = sortedModules.stream().map(m -> m.getName().toString()).toList();
      assertThat(
          "Export relations should be: mainMod --> syn1Mod --> syn2Mod --> aMod, but was: "
              + sortedModNames,
          sortedModules,
          contains(aRuntimeMod, syn2RuntimeMod, syn1RuntimeMod, mainRuntimeMod));
    }
  }

  private static Context createContext(Path projDir) {
    return ContextUtils.defaultContextBuilder()
        .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
        .build();
  }

  private static void compile(Context ctx) {
    var polyCtx = new PolyglotContext(ctx);
    polyCtx.getTopScope().compile(true);
  }

  private static Module getLoadedModule(Context ctx, String modName) {
    var mod = ModuleUtils.getLoadedModule(ctx, modName);
    assert mod != null;
    return mod;
  }

  private static List<Module> runExportsResolutionSort(List<Module> modules, Context ctx) {
    var ensoCtx = ContextUtils.leakContext(ctx);
    var compilerCtx = ensoCtx.getCompiler().context();
    var exportsResolution = new ExportsResolution(compilerCtx);
    var compilerModules = modules.stream().map(Module::asCompilerModule).toList();
    var sortedCompilerModules =
        exportsResolution.runSort(CollectionConverters.asScala(compilerModules).toList());
    return CollectionConverters.asJava(sortedCompilerModules).stream()
        .map(Module::fromCompilerModule)
        .toList();
  }
}
