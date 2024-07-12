package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ModuleUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ExportModuleTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void exportSubModuleFromExistingModule() throws IOException {
    var subModule =
        new SourceModule(
            QualifiedName.fromString("Module.SubModule"),
            """
        # Blank on purpose
        """);
    var module =
        new SourceModule(
            QualifiedName.fromString("Module"),
            """
        # Blank on purpose - ensures that `Module` is not just synthetic
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.Module.SubModule
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(subModule, module, mainMod), projDir);
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var mainModExportedSymbols = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainModExportedSymbols.size(), is(1));
      assertThat(mainModExportedSymbols, hasKey("SubModule"));
      assertThat(mainModExportedSymbols.get("SubModule").size(), is(1));
      assertThat(
          mainModExportedSymbols.get("SubModule").get(0), is(instanceOf(ResolvedModule.class)));
    }
  }

  @Test
  public void exportSubModuleFromSyntheticModule() throws IOException {
    var subModule =
        new SourceModule(
            QualifiedName.fromString("Module.SubModule"),
            """
        # Blank on purpose
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.Module.SubModule
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(subModule, mainMod), projDir);
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var mainModExportedSymbols = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainModExportedSymbols.size(), is(1));
      assertThat(mainModExportedSymbols, hasKey("SubModule"));
      assertThat(mainModExportedSymbols.get("SubModule").size(), is(1));
      assertThat(
          mainModExportedSymbols.get("SubModule").get(0), is(instanceOf(ResolvedModule.class)));
    }
  }
}
