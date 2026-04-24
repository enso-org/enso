package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.nio.file.Path;
import java.util.Set;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class HelloWorldCacheTest {
  @Rule public TemporaryFolder tmpDir = new TemporaryFolder();

  @Test
  public void loadingHelloWorldTwiceUsesCaching() throws Exception {
    var root = new File("../..").getAbsoluteFile();
    assertTrue("build.sbt exists at " + root, new File(root, "build.sbt").exists());
    var helloWorld = children(root, "test", "Benchmarks", "src", "Startup", "Hello_World.enso");
    assertTrue("Hello_World.enso found", helloWorld.exists());

    // the first run may or may not use caches
    var firstMsgs = executeOnce(helloWorld);
    assertTrue("Contains hello world:\n" + firstMsgs, firstMsgs.endsWith("Hello World"));
    // after the first run the caches for Hello_World.enso must be generated

    // the second run must read Hello_World from its .ir file!
    var secondMsgs = executeOnce(helloWorld);
    assertTrue("Contains hello world:\n" + secondMsgs, secondMsgs.contains("Hello World"));
    assertThat(
        "Properly deserialized:\n" + secondMsgs,
        secondMsgs,
        allOf(
            containsString("Deserializing module"),
            containsString("Hello_World"),
            containsString("from IR file: true")));
  }

  @Test
  public void irCacheCannotBeEnabled_WhenPrivateCheckIsDisabled() {
    try (var ctx =
        ContextUtils.newBuilder()
            .assertGC(false)
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_PRIVATE_CHECK, "true")
                        .option(RuntimeOptions.DISABLE_IR_CACHES, "false"))
            .build()) {
      try {
        ctx.context().initialize(LanguageInfo.ID);
        fail("Context initialization should fail");
      } catch (Exception e) {
        assertThat(
            e.getMessage(),
            allOf(
                containsString("private check is disabled"),
                containsString("IR caching is enabled")));
      }
    }
  }

  @Test
  public void runningAfterPrivateCheckWasDisabled_ShouldFail() throws Exception {
    var libDir = tmpDir.newFolder("Lib").toPath();
    var projDir = tmpDir.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Priv_Mod"),
                """
                private
                priv_func x = x
                """),
            new SourceModule(QualifiedName.fromString("Main"), "# Intentionally empty")),
        libDir);

    ProjectUtils.createProject(
        "Proj",
        """
        import local.Lib.Priv_Mod
        main =
            Priv_Mod.priv_func 42
        """,
        projDir);
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");

    // First run with private check DISABLED
    try (var privateCheckDisabledCtx = ctxInProj(projDir, true).build()) {
      var polyCtx = new PolyglotContext(privateCheckDisabledCtx.context());
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocMainModType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
      var res = mainMethod.execute();
      assertThat("Eval with private check disabled is OK", res.asInt(), is(42));
      polyCtx = null;
      mainMod = null;
      assocMainModType = null;
      mainMethod = null;
      res = null;
    }

    // Second run with private check ENABLED - should fail to compile
    try (var privateCheckEnabledCtx = ctxInProj(projDir, false).build()) {
      try {
        privateCheckEnabledCtx.topScope().compile(true);
        fail("Should result in compilation error");
      } catch (PolyglotException e) {
        assertThat(e.getMessage(), containsString("Cannot import private module"));
        e = null;
      }
    }
  }

  private static ContextUtils.Builder ctxInProj(Path projRoot, boolean disablePrivateCheck) {
    return ContextUtils.newBuilder()
        .withModifiedContext(
            bldr ->
                bldr.option(
                        RuntimeOptions.DISABLE_PRIVATE_CHECK,
                        disablePrivateCheck ? "true" : "false")
                    .option(
                        RuntimeOptions.DISABLE_IR_CACHES, disablePrivateCheck ? "true" : "false")
                    .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true"))
        .withProjectRoot(projRoot);
  }

  private static String executeOnce(File src) throws Exception {
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.LOG_LEVEL, Level.FINE.getName())
                        .option(RuntimeOptions.CHECK_CWD, "false")
                        .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.PROJECT_ROOT, findBenchmarks(src).getAbsolutePath())
                        .option(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS, "true"))
            .build()) {
      var code = Source.newBuilder("enso", src).build();
      var res = ctx.evalModule(code, "main");
      assertTrue("Result of IO.println is Nothing", res.isNull());
      res = null;
      code = null;
      return ctx.getOut()
          .lines()
          .filter(l -> l.toUpperCase().contains("HELLO"))
          .collect(Collectors.joining("\n"));
    }
  }

  private static File children(File f, String... names) {
    for (var n : names) {
      f = new File(f, n);
    }
    return f;
  }

  private static File findBenchmarks(File f) {
    for (; ; ) {
      if (f.getName().equals("Benchmarks")) {
        return f;
      }
      f = f.getParentFile();
    }
  }
}
