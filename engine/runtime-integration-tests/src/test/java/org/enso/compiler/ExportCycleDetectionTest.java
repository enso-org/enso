package org.enso.compiler;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
import org.hamcrest.Matcher;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ExportCycleDetectionTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void detectCycleInTwoModules() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        from project.B_Module export B_Type
        type A_Type
        """);
    var bMod =
        new SourceModule(
            QualifiedName.fromString("B_Module"),
            """
        from project.A_Module export A_Type
        type B_Type
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
                import project.A_Module
                import project.B_Module
                """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, bMod, mainMod), projDir);
    expectProjectCompilationError(
        projDir,
        allOf(
            containsString("Export statements form a cycle"),
            containsString("local.Proj.B_Module exports local.Proj.A_Module"),
            containsString("which exports local.Proj.B_Module")));
  }

  @Test
  public void detectCycleInThreeModules() throws IOException {
    var aMod =
        new SourceModule(
            QualifiedName.fromString("A_Module"),
            """
        from project.B_Module export C_Type
        type A_Type
        """);
    var bMod =
        new SourceModule(
            QualifiedName.fromString("B_Module"),
            """
        from project.C_Module export C_Type
        """);
    var cMod =
        new SourceModule(
            QualifiedName.fromString("C_Module"),
            """
        from project.A_Module export A_Type
        type C_Type
        """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
                import project.A_Module
                import project.B_Module
                import project.C_Module
                """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(aMod, bMod, cMod, mainMod), projDir);
    expectProjectCompilationError(
        projDir,
        allOf(
            containsString("Export statements form a cycle"),
            containsString("local.Proj.B_Module exports local.Proj.C_Module"),
            containsString("which exports local.Proj.A_Module"),
            containsString("which exports local.Proj.B_Module")));
  }

  private void expectProjectCompilationError(Path projDir, Matcher<String> errMsgMatcher) {
    var out = new ByteArrayOutputStream();
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .out(out)
            .err(out)
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      try {
        polyCtx.getTopScope().compile(true);
        fail("Expected compilation error");
      } catch (PolyglotException e) {
        assertThat(e.getMessage(), containsString("Compilation aborted"));
      }
    }
    assertThat(out.toString(), errMsgMatcher);
  }
}
