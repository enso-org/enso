package org.enso.interpreter.test.imports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Set;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ModuleUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ImportSymbolsTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void importAllFromModuleDoesNotImportModuleItself() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_module"), """
        a_mod_method x = x
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.A_module import all
        main =
            A_Module.a_mod_method 42
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
    var out = new ByteArrayOutputStream();
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .out(out)
            .err(out)
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      try {
        polyCtx.getTopScope().compile(true);
        fail("Compilation error expected. out = " + out);
      } catch (PolyglotException e) {
        assertThat(e.getMessage(), containsString("The name `A_Module` could not be found"));
      }
    }
  }

  @Test
  public void importAllFromTypeDoesNotImportTypeItself() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_module"),
            """
        type A_Type
            Cons
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from project.A_module.A_Type import all
        main =
            A_Type.Cons
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, mainMod), projDir);
    var out = new ByteArrayOutputStream();
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .out(out)
            .err(out)
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      try {
        polyCtx.getTopScope().compile(true);
        fail("Compilation error expected. Captured output = " + out);
      } catch (PolyglotException e) {
        assertThat(e.getMessage(), containsString("The name `A_Type` could not be found"));
      }
    }
  }

  // TODO: Tracked by https://github.com/enso-org/enso/issues/10504
  @Ignore
  @Test
  public void importEntityFromModuleThatExportsItFromOtherModule() throws IOException {
    var aMod =
        new SourceModule(QualifiedName.fromString("A_Module"), """
        type A_Type
        """);
    var bMod =
        new SourceModule(
            QualifiedName.fromString("B_Module"),
            """
        export project.A_Module.A_Type
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        import project.B_Module.A_Type
        """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, bMod, mainMod), projDir);
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      var mainModResolvedImps = ModuleUtils.getResolvedImports(ctx, "local.Proj.Main");
      assertThat(mainModResolvedImps.size(), is(1));
      assertThat(
          "There should be only one target of Main's resolved import",
          mainModResolvedImps.get(0).targets().size(),
          is(1));
      assertThat(
          "Resolved import target of Main should point to A_Type",
          mainModResolvedImps.get(0).targets().apply(0),
          is(instanceOf(ResolvedType.class)));
    }
  }
}
