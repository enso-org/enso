package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.compiler.data.BindingsMap.ResolvedConversionMethod;
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

public class ExportConversionMethodTest {

  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void conversionMethodCanBeImportedByName() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        type A_Type
            Value
        type B_Type
        """);
    var bMod =
        new SourceModule(
            QualifiedName.fromString("B_Module"),
            """
        import project.A_Module.A_Type
        import project.A_Module.B_Type

        B_Type.from (_:A_Type) = 42
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"), """
        import project.B_Module.from
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, bMod, mainMod), projDir);
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);

      var mainResolvedImps = ModuleUtils.getResolvedImports(ctx, "local.Proj.Main");
      assertThat(mainResolvedImps.size(), is(1));
      assertThat(mainResolvedImps.get(0).targets().size(), is(1));
      assertThat(
          mainResolvedImps.get(0).targets().head(), is(instanceOf(ResolvedConversionMethod.class)));
    }
  }

  @Test
  public void conversionMethodIsInBindingMap() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        type A_Type
        type B_Type
        B_Type.from (_:A_Type) = 42
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"), """
        export project.A_Module.from
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);

      var aModExportedSymbols =
          ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.A_Module");
      assertThat(aModExportedSymbols.size(), is(3));
      assertThat(aModExportedSymbols.keySet(), containsInAnyOrder("A_Type", "B_Type", "from"));

      var mainResolvedImps = ModuleUtils.getResolvedImports(ctx, "local.Proj.Main");
      assertThat(mainResolvedImps.size(), is(1));
      assertThat(mainResolvedImps.get(0).targets().size(), is(1));
      assertThat(
          mainResolvedImps.get(0).targets().head(), is(instanceOf(ResolvedConversionMethod.class)));
      var mainExportedSyms = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSyms.size(), is(1));
      assertThat(mainExportedSyms, hasKey("from"));
    }
  }

  @Test
  public void multipleConversionMethodsCanBeImported() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        type A_Type
        type B_Type
        B_Type.from (_:A_Type) = 1
        A_Type.from (_:B_Type) = 2
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"), """
        export project.A_Module.from
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);

      var aModExportedSymbols =
          ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.A_Module");
      assertThat(aModExportedSymbols.size(), is(3));
      assertThat(aModExportedSymbols.keySet(), containsInAnyOrder("A_Type", "B_Type", "from"));

      var mainResolvedImps = ModuleUtils.getResolvedImports(ctx, "local.Proj.Main");
      assertThat(mainResolvedImps.size(), is(1));
      assertThat(mainResolvedImps.get(0).targets().size(), is(2));
      assertThat(
          mainResolvedImps.get(0).targets().apply(0),
          is(instanceOf(ResolvedConversionMethod.class)));
      assertThat(
          mainResolvedImps.get(0).targets().apply(1),
          is(instanceOf(ResolvedConversionMethod.class)));
      var mainExportedSyms = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainExportedSyms.size(), is(1));
      assertThat(mainExportedSyms, hasKey("from"));
      assertThat(mainExportedSyms.get("from").size(), is(2));
    }
  }
}
