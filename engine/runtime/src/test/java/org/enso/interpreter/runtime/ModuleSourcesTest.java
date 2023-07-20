package org.enso.interpreter.runtime;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.enso.text.buffer.Rope$;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ModuleSourcesTest {

  private File f;
  private Context ctx;

  @Before
  public void prepareTest() throws IOException {

    f = File.createTempFile("module-sources", ".enso");
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .logHandler(new ByteArrayOutputStream())
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    this.ctx = Context.newBuilder().engine(eng).allowIO(IOAccess.ALL).allowAllAccess(true).build();
  }

  @After
  public void cleanup() {
    f.delete();
    this.ctx.close();
  }

  @Test
  public void moduleSourcesWithFile() {
    var sources = ModuleSources.NONE;
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);
    var sourcesWithFile = sources.newWith(tFile);
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("rope is null", sourcesWithFile.rope() == null);
  }

  @Test
  public void moduleSourcesWithRopePreservesFile() {
    var sources = ModuleSources.NONE;
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);
    var rope = Rope$.MODULE$.apply("foo");
    var sourcesWithFile = sources.newWith(tFile).newWith(rope);
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("rope is non-null", sourcesWithFile.rope() == rope);
  }

  @Test
  public void modulesSourcesResetPreservesFile() {
    var sources = ModuleSources.NONE;
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);
    var rope = Rope$.MODULE$.apply("foo");
    var sourcesWithFile = sources.newWith(tFile).newWith(rope).reset();
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("getPath is null", sourcesWithFile.rope() == null);
  }
}
