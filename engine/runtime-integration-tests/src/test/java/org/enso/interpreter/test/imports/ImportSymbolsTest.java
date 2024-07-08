package org.enso.interpreter.test.imports;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
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
            QualifiedName.fromString("A_module"), """
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
}
