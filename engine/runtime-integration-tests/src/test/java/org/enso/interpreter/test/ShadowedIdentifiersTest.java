package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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
import org.hamcrest.Matcher;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ShadowedIdentifiersTest  {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void shadowingTypeFromDifferentModuleIsWarning() throws IOException {
    var modSrc = """
        type T
        """;
    var mainSrc = """
        from project.Mod import T
        type T
        """;
    var projDir = createProjectWithTwoModules(mainSrc, modSrc);
    testProjectCompilationWarning(projDir, allOf(containsString("shadowed")));
  }

  @Test
  public void shadowingStaticMethodFromDifferentModuleIsWarning() throws IOException {
    var modSrc = """
        stat_method x y = x + y
        """;
    var mainSrc =
        """
        from project.Mod import stat_method
        stat_method x = x + 42
        """;
    var projDir = createProjectWithTwoModules(mainSrc, modSrc);
    testProjectCompilationWarning(projDir, allOf(containsString("shadowed")));
  }

  @Test
  public void shadowingRenamedTypeFromDifferentModuleIsWarning() throws IOException {
    var modSrc = """
        type R
        """;
    var mainSrc = """
        import project.Mod.R as T
        type T
        """;
    var projDir = createProjectWithTwoModules(mainSrc, modSrc);
    testProjectCompilationWarning(projDir, allOf(containsString("shadowed")));
  }

  @Test
  public void shadowingRenamedStaticMethodFromDifferentModuleIsWarning() throws IOException {
    var modSrc = """
        stat_method_foo x y = x + y
        """;
    var mainSrc =
        """
        import project.Mod.stat_method_foo as stat_method
        stat_method x = x + 42
        """;
    var projDir = createProjectWithTwoModules(mainSrc, modSrc);
    testProjectCompilationWarning(projDir, allOf(containsString("shadowed")));
  }

  @Test
  public void shadowedTypeHasPrecedence() throws IOException {
    var modSrc = """
        type T
            foo = "Mod.T.foo"
        """;
    var mainSrc =
        """
        from project.Mod import T
        type T
            foo = "Main.T.foo"
        main =
            T.foo
        """;
    var projDir = createProjectWithTwoModules(mainSrc, modSrc);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isString(), is(true));
          assertThat(res.asString(), is("Main.T.foo"));
        });
  }

  @Test
  public void shadowedTypeHasPrecedenceInRuntimeTyping() throws IOException {
    var modSrc =
        """
        type T
            Value
            foo self = "Mod.T.foo"
        """;
    var mainSrc =
        """
        from project.Mod import T

        type T
            Value
            foo self = "Main.T.foo"

        bar (t:T) = t.foo

        main =
            t = T.Value
            bar t
        """;
    var projDir = createProjectWithTwoModules(mainSrc, modSrc);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isString(), is(true));
          assertThat(res.asString(), is("Main.T.foo"));
        });
  }

  private Path createProjectWithTwoModules(String mainSrc, String modSrc) throws IOException {
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(QualifiedName.fromString("Main"), mainSrc),
            new SourceModule(QualifiedName.fromString("Mod"), modSrc)),
        projDir);
    return projDir;
  }

  private void testProjectCompilationWarning(Path mainProjDir, Matcher<String> warnMessageMatcher) {
    var out = new ByteArrayOutputStream();
    try (var ctx =
        ContextUtils.defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, mainProjDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .out(out)
            .err(out)
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      polyCtx.getTopScope().compile(true);
      assertThat(out.toString(), warnMessageMatcher);
    }
  }
}
