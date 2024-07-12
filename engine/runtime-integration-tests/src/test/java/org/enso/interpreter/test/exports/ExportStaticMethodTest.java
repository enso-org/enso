package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasKey;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
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

public class ExportStaticMethodTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void staticMethodCanBeExportedByName() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"), """
        static_method x = x
        """);
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        export project.T_Module.static_method
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.A_Module import all
        main =
            static_method 42
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(tMod, aMod, mainMod), projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(42));
        });
  }

  @Test
  public void staticAndModuleMethodsWithSameNameCanBeImported() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"),
            """
        type My_Type
            method x = x
        method x = x
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.T_Module import My_Type, method
        main =
            My_Type.method 42 == method 42
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(tMod, mainMod), projDir);

    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isBoolean(), is(true));
          assertThat(res.asBoolean(), is(true));
        });
  }

  @Test
  public void moduleMethodIsInBindingMap() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"), """
        module_method x = x
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.T_Module.module_method
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(tMod, mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var mainModExportedSymbols = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainModExportedSymbols.size(), is(1));
      assertThat(mainModExportedSymbols, hasKey("module_method"));
    }
  }

  @Test
  public void staticMethodIsInBindingMap() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"),
            """
        type My_Type
        My_Type.static_method x = x
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.T_Module.static_method
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(tMod, mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var mainModExportedSymbols = ModuleUtils.getExportedSymbolsFromModule(ctx, "local.Proj.Main");
      assertThat(mainModExportedSymbols.size(), is(1));
      assertThat(mainModExportedSymbols, hasKey("static_method"));
    }
  }

  @Test
  public void staticMethodIsDefinedEntity() throws IOException {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"), """
        static_method x = x
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mainMod), projDir);

    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var definedEntities = ModuleUtils.getDefinedEntities(ctx, "local.Proj.Main");
      assertThat(definedEntities.size(), is(1));
      assertThat(definedEntities.get(0).name(), containsString("static_method"));
    }
  }
}
