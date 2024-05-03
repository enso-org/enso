package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.allOf;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.enso.interpreter.CompilationAbortedException;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.PolyglotException;
import org.hamcrest.Matcher;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Shadowing identifiers of imported symbols should result in a compilation failure. This testing
 * suite is focused on testing the shadowing of identifiers in various context of multiple modules,
 * not just a single module.
 */
public class ExtensionMethodResolutionTest extends TestBase {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();
  private static final Matcher<String> methodsOverloadErrorMessageMatcher =
      allOf(
          containsString("Method overloads are not supported"),
          containsString("defined multiple times"));

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
  public void extensionMethodAndNormalMethodConflict() throws IOException {
    var src = """
        type T
            foo x = x
        T.foo x y = x + y
        """;
    testProjectCompilationFailure(src, methodsOverloadErrorMessageMatcher);
  }

  @Test
  public void extensionMethodAndNormalMethodConflictInDifferentModules() throws IOException {
    var modSrc = """
        type T
            foo x = x
        """;
    var mainSrc = """
        from project.Mod import T
        T.foo x y = x + y
        """;
    var projDir = createProject("Proj", mainSrc, tempFolder);
    var modSrcFile = projDir.resolve("src").resolve("Mod.enso");
    Files.writeString(modSrcFile, modSrc);
    testProjectCompilationFailure(projDir, methodsOverloadErrorMessageMatcher);
  }

  @Test
  public void extensionMethodAndNormalMethodConflictInThreeModules() throws IOException {
    var tSrc = """
        type T
        """;
    var modSrc = """
        from project.T import T
        T.foo x = x
        """;
    var mainSrc =
        """
        from project.T import T
        # Make sure we import also the extension method from Mod
        from project.Mod import all
        T.foo x y = x + y
        """;
    var projDir = createProject("Proj", mainSrc, tempFolder);
    var tSrcFile = projDir.resolve("src").resolve("T.enso");
    Files.writeString(tSrcFile, tSrc);
    var modSrcFile = projDir.resolve("src").resolve("Mod.enso");
    Files.writeString(modSrcFile, modSrc);
    testProjectCompilationFailure(projDir, methodsOverloadErrorMessageMatcher);
  }

  @Test
  public void extensionMethodAndNormalMethodConflictInDifferentProjects() throws IOException {
    var libSrc = """
        type T
            foo x = x
        """;
    createProject("Lib", libSrc, tempFolder);
    var mainSrc = """
        from local.Lib import T
        T.foo x y = x + y
        """;
    var mainProjDir = createProject("Main", mainSrc, tempFolder);
    testProjectCompilationFailure(mainProjDir, methodsOverloadErrorMessageMatcher);
  }

  private void testProjectCompilationFailure(String mainSrc, Matcher<String> errorMessageMatcher)
      throws IOException {
    var projDir = createProject("Proj", mainSrc, tempFolder);
    testProjectCompilationFailure(projDir, errorMessageMatcher);
  }

  private void testProjectCompilationFailure(
      Path mainProjDir, Matcher<String> errorMessageMatcher) {
    var out = new ByteArrayOutputStream();
    try (var ctx =
        defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, mainProjDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .out(out)
            .err(out)
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      try {
        polyCtx.getTopScope().compile(true);
        fail("Expected compilation error: " + out);
      } catch (PolyglotException e) {
        assertThat(e.getMessage(), is(CompilationAbortedException.COMPILATION_ABORTED_MSG));
        assertThat(out.toString(), errorMessageMatcher);
      }
    }
  }
}
