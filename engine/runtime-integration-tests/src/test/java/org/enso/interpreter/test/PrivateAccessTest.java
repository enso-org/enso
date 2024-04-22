package org.enso.interpreter.test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.PolyglotException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class PrivateAccessTest extends TestBase {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  @Test
  public void privateConstructorCanBeCalledInUnknownProject() {
    var src =
        """
        type My_Type
            private Cons data
        main =
            obj = My_Type.Cons 42
            obj.data
        """;
    try (var ctx = createDefaultContext()) {
      var res = TestBase.evalModule(ctx, src);
      assertThat(res.isNumber(), is(true));
      assertThat(res.asInt(), is(42));
    }
  }

  @Test
  public void privateFieldIsNotExposedToPolyglot() throws IOException {
    var mainSrc =
        """
        type My_Type
            private Cons data
        main = My_Type.Cons 42
        """;
    var projDir = createProject("My_Project", mainSrc);
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
    try (var ctx =
        defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocType, "main").get();
      var res = mainMethod.execute();
      assertThat(res.hasMember("data"), is(false));
    }
  }

  @Test
  public void privateConstructorIsNotExposedToPolyglot() throws IOException {
    var mainSrc = """
        type My_Type
            private Cons data
        """;
    var projDir = createProject("My_Project", mainSrc);
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
    try (var ctx =
        defaultContextBuilder()
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var myType = mainMod.getType("My_Type");
      assertThat(myType.hasMember("Cons"), is(false));
    }
  }

  /**
   * Creates temporary project directory structure with a given main source content. No need to
   * clean it up, as it is managed by JUnit TemporaryFolder rule. Note that we need to create a
   * project, otherwise the private stuff won't work.
   *
   * @param projName Name of the project (as defined in package.yaml).
   * @param mainSrc Main.enso source content
   * @return Path to the newly created directly structure - a project directory.
   */
  private Path createProject(String projName, String mainSrc) throws IOException {
    var projDir = tempFolder.newFolder(projName);
    assert projDir.exists();
    var projYaml =
        """
name: %s
version: 0.0.1
prefer-local-libraries: true
        """.formatted(projName);
    var yamlPath = projDir.toPath().resolve("package.yaml");
    Files.writeString(yamlPath, projYaml);
    assert yamlPath.toFile().exists();
    var srcDir = tempFolder.newFolder(projName, "src");
    assert srcDir.exists();
    var mainSrcPath = srcDir.toPath().resolve("Main.enso");
    Files.writeString(mainSrcPath, mainSrc);
    assert mainSrcPath.toFile().exists();
    return projDir.toPath();
  }
}
