package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.logging.Level;
import org.enso.common.RuntimeOptions;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class IRCacheLocationTest {
  @Rule public TemporaryFolder tmpDir = new TemporaryFolder();

  @BeforeClass
  public static void skipOnWindows() {
    Assume.assumeFalse(
        "This test suite should be skipped on Windows, because of NTFS weird refresh rate",
        System.getProperty("os.name").toLowerCase().contains("win"));
  }

  @Test
  public void noGlobalCacheOption_IsRespected() throws IOException {
    var projDir = tmpDir.newFolder();
    ProjectUtils.createProject(
        "Proj",
        """
        main =
            42
        """,
        projDir.toPath());
    var mainSrcPath = projDir.toPath().resolve("src").resolve("Main.enso");

    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.LOG_LEVEL, Level.FINE.getName())
                        .option(RuntimeOptions.CHECK_CWD, "false")
                        .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true"))
            .withProjectRoot(projDir.toPath())
            .build()) {
      var polyCtx = new PolyglotContext(ctx.context());
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocMainModType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
      var res = mainMethod.execute();
      assertThat("Module evaluation is OK", res.asInt(), is(42));
    }

    var cacheDir = projDir.toPath().resolve(".enso");
    assertThat("Cache dir was not created in project", cacheDir.toFile().exists(), is(false));
  }

  @Test
  public void irCacheIsNotCreatedForDependencies() throws IOException {
    var libDir = tmpDir.newFolder("Lib");
    ProjectUtils.createProject(
        "Lib",
        """
        lib_method =
            42
        """,
        libDir.toPath());

    var projDir = tmpDir.newFolder("Proj");
    ProjectUtils.createProject(
        "Proj",
        """
        from local.Lib import lib_method

        main =
            lib_method
        """,
        projDir.toPath());
    var mainSrcPath = projDir.toPath().resolve("src").resolve("Main.enso");

    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.LOG_LEVEL, Level.FINE.getName())
                        .option(RuntimeOptions.CHECK_CWD, "false")
                        .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true"))
            .withProjectRoot(projDir.toPath())
            .build()) {
      var polyCtx = new PolyglotContext(ctx.context());
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocMainModType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
      Value res;
      try {
        res = mainMethod.execute();
      } catch (PolyglotException e) {
        throw new AssertionError(
            "Unexpected exception during module evaluation: "
                + e.getMessage()
                + "\n"
                + "out: \n"
                + ctx.getOut(),
            e);
      }
      assertThat("Module evaluation is OK", res.asInt(), is(42));
    }

    var libCacheDir = libDir.toPath().resolve(".enso");
    assertThat("Cache dir for Lib was not created", libCacheDir.toFile().exists(), is(false));
  }
}
