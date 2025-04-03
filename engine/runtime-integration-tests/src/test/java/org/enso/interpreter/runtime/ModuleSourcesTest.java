package org.enso.interpreter.runtime;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtilsRule;
import org.enso.text.buffer.Rope$;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class ModuleSourcesTest {

  @ClassRule
  public static final ContextUtilsRule ctxRule =
      ContextUtilsRule.createCustom(ModuleSourcesTest::createCtx);

  private File f;

  private static Context createCtx() {
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    var ctx = Context.newBuilder().engine(eng).allowIO(IOAccess.ALL).allowAllAccess(true).build();
    return ctx;
  }

  @Before
  public void prepareTest() throws IOException {
    f = File.createTempFile("module-sources", ".enso");
  }

  @After
  public void cleanup() {
    f.delete();
  }

  @Test
  public void moduleSourcesWithFile() {
    var sources = ModuleSources.NONE;
    var ensoContext = ctxRule.leakContext();
    var tFile = ensoContext.getTruffleFile(f);
    var sourcesWithFile = sources.newWith(tFile);
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("rope is null", sourcesWithFile.rope() == null);
  }

  @Test
  public void moduleSourcesWithRopePreservesFile() {
    var sources = ModuleSources.NONE;
    var ensoContext = ctxRule.leakContext();
    var tFile = ensoContext.getTruffleFile(f);
    var rope = Rope$.MODULE$.apply("foo");
    var sourcesWithFile = sources.newWith(tFile).newWith(rope);
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("rope is non-null", sourcesWithFile.rope() == rope);
  }

  @Test
  public void modulesSourcesResetPreservesFile() {
    var sources = ModuleSources.NONE;
    var ensoContext = ctxRule.leakContext();
    var tFile = ensoContext.getTruffleFile(f);
    var rope = Rope$.MODULE$.apply("foo");
    var sourcesWithFile = sources.newWith(tFile).newWith(rope).reset();
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("getPath is null", sourcesWithFile.rope() == null);
  }
}
