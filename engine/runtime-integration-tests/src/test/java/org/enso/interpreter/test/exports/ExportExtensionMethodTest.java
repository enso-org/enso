package org.enso.interpreter.test.exports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.compiler.data.BindingsMap.DefinedEntity;
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

public class ExportExtensionMethodTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void extensionMethodCanBeExportedByName() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"),
            """
        type My_Type
            Value x
        My_Type.extension_method self = self.x
        """);
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        export project.T_Module.My_Type
        export project.T_Module.extension_method
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.A_Module import all
        main =
            obj = My_Type.Value 42
            obj.extension_method
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
  public void multipleExtensionMethodsCanBeExportedByName() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"),
            """
        type My_Type
            Value x
        type My_Other_Type
            Value y
        My_Type.extension_method self = self.x
        My_Other_Type.extension_method self = self.y
        """);
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        export project.T_Module.My_Type
        export project.T_Module.My_Other_Type
        export project.T_Module.extension_method
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.A_Module import all
        main =
            t = My_Type.Value 42
            ot = My_Other_Type.Value 42
            t.extension_method == ot.extension_method
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(tMod, aMod, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isBoolean(), is(true));
          assertThat(res.asBoolean(), is(true));
        });
  }

  @Test
  public void extensionMethodIsInBindingMap() throws IOException {
    var tMod =
        new SourceModule(
            QualifiedName.fromString("T_Module"),
            """
        type My_Type
            Value x
        My_Type.extension_method self = self.x
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        export project.T_Module.My_Type
        export project.T_Module.extension_method
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
      assertThat(mainModExportedSymbols.size(), is(2));
      assertThat(
          mainModExportedSymbols.keySet(), containsInAnyOrder("My_Type", "extension_method"));
    }
  }

  @Test
  public void extensionMethodIsDefinedEntity() throws IOException {
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        type My_Type
            Value x
        My_Type.extension_method self = self.x
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
      assertThat(definedEntities.isEmpty(), is(false));
      var entityNames = definedEntities.stream().map(DefinedEntity::name).toList();
      assertThat(entityNames, hasItem("extension_method"));
    }
  }
}
