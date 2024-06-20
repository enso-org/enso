package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.enso.polyglot.TopScope;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
import org.hamcrest.Matcher;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Shadowing identifiers from imported modules is not a compilation error, neither runtime error.
 * But we need to make sure that the shadowing is deterministic and that the shadowing is not
 * affecting the resolution of extension methods. In other words, we need to make sure that the
 * method resolution is deterministic.
 */
public class ExtensionMethodResolutionTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();
  private static final Matcher<String> methodsOverloadErrorMessageMatcher =
      allOf(
          containsString("Method overloads are not supported"),
          containsString("defined multiple times"));

  private static final Matcher<String> ambiguousResolutionErrorMessageMatcher =
      allOf(
          containsString("resolved ambiguously to"),
          containsString("The symbol was first resolved to"));

  @Test
  public void twoExtensionMethodsWithSameNameInOneModuleShouldFail() throws IOException {
    var src = """
        type T
        T.foo x = x
        T.foo x y = x + y
        """;
    testProjectCompilationFailure(src, methodsOverloadErrorMessageMatcher);
  }

  @Test
  public void extensionMethodAndNormalMethodConflictInOneModule() throws IOException {
    var src = """
        type T
            foo x = x
        T.foo x y = x + y
        """;
    testProjectCompilationFailure(src, methodsOverloadErrorMessageMatcher);
  }

  @Test
  public void firstResolutionIsInTypesScope() throws IOException {
    var xMod =
        new SourceModule(
            QualifiedName.fromString("X"),
            """
            type T
            T.foo = "X"
            """);
    var yMod =
        new SourceModule(
            QualifiedName.fromString("Y"),
            """
            from project.X import T
            T.foo = "Y"
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.X import T
            from project.Y import all
            T.foo = "Main"
            main = T.foo
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(xMod, yMod, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(
              "Method should be first resolved in the types module scope", res.asString(), is("X"));
        });
  }

  @Test
  public void secondResolutionIsInCurrentModuleScope() throws IOException {
    var xMod =
        new SourceModule(QualifiedName.fromString("X"), """
            type T
            """);
    var yMod =
        new SourceModule(
            QualifiedName.fromString("Y"),
            """
            from project.X import T
            T.foo = "Y"
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.X import T
            from project.Y import all
            T.foo = "Main"
            main = T.foo
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(xMod, yMod, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(
              "Method should be secondly resolved in the current scope",
              res.asString(),
              is("Main"));
        });
  }

  @Test
  public void resolutionFromImportedModulesIsCompilerError1() throws IOException {
    var xMod =
        new SourceModule(QualifiedName.fromString("X"), """
            type T
            """);
    var yMod =
        new SourceModule(
            QualifiedName.fromString("Y"),
            """
            from project.X import T
            T.foo = "Y"
            """);
    var zMod =
        new SourceModule(
            QualifiedName.fromString("Z"),
            """
            from project.X import T
            T.foo = "Z"
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.X import T
            from project.Y import all
            from project.Z import all
            main = T.foo
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(xMod, yMod, zMod, mainMod), projDir);
    testProjectCompilationFailure(projDir, ambiguousResolutionErrorMessageMatcher);
  }

  @Test
  public void resolutionFromImportedModulesIsCompilerError2() throws IOException {
    var xMod =
        new SourceModule(QualifiedName.fromString("X"), """
            type T
            """);
    var yMod =
        new SourceModule(
            QualifiedName.fromString("Y"),
            """
            from project.X import T
            T.foo = "Y"
            """);
    var zMod =
        new SourceModule(
            QualifiedName.fromString("Z"),
            """
            from project.X import T
            T.foo = "Z"
            """);
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.X import T
            from project.Z import all
            from project.Y import all
            main = T.foo
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(xMod, yMod, zMod, mainMod), projDir);
    testProjectCompilationFailure(projDir, ambiguousResolutionErrorMessageMatcher);
  }

  @Test
  public void sameMethodInShadowedType() throws IOException {
    var mod =
        new SourceModule(
            QualifiedName.fromString("Mod"),
            """
            type T
                method = 42
            """);
    // This is type-shadowing, which is allowed.
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            from project.Mod import T
            type T
                method = 23
            main =
                T.method
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mod, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(23));
        });
  }

  @Test
  public void sameExtensionMethodInDifferentTypes() throws IOException {
    var mod =
        new SourceModule(
            QualifiedName.fromString("Mod"),
            """
            type T
            T.method = 42
            """);
    // main module imports just the `Mod` and not the type - it should succeed.
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            import project.Mod
            type T
            T.method = 23
            main =
                T.method
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mod, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(23));
        });
  }

  @Test
  public void sameExtensionMethodInDifferentTypesInThreeModules() throws IOException {
    var mod2 =
        new SourceModule(
            QualifiedName.fromString("Mod2"), """
            # An empty module
            """);
    // The type T defined in mod1 and mainMod have exactly the same location on purpose.
    var mod1 =
        new SourceModule(
            QualifiedName.fromString("Mod1"),
            """
            import project.Mod2
            type T
            T.method = 1
            """);
    // main module imports just the `Mod` and not the type - it should succeed.
    var mainMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
            import project.Mod1
            type T
            T.method = 2
            main =
                T.method
            """);
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", Set.of(mod2, mod1, mainMod), projDir);
    ProjectUtils.testProjectRun(
        projDir,
        res -> {
          assertThat(res.isNumber(), is(true));
          assertThat(res.asInt(), is(2));
        });
  }

  private void testProjectCompilationFailure(String mainSrc, Matcher<String> errorMessageMatcher)
      throws IOException {
    var projDir = tempFolder.newFolder().toPath();
    ProjectUtils.createProject("Proj", mainSrc, projDir);
    testProjectCompilationFailure(projDir, errorMessageMatcher);
  }

  private void testProjectCompilationFailure(
      Path mainProjDir, Matcher<String> errorMessageMatcher) {
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
      TopScope topScope = polyCtx.getTopScope();
      try {
        topScope.compile(true);
        fail("Expected compilation error: " + out);
      } catch (PolyglotException e) {
        assertThat(
            "Exception should be a syntax error, but instead is " + e.getMessage(),
            e.isSyntaxError(),
            is(true));
        assertThat(out.toString(), errorMessageMatcher);
      }
    }
  }
}
