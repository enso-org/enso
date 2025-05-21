package org.enso.interpreter.runtime;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import org.enso.test.utils.ContextUtils;
import org.enso.text.buffer.Rope$;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class ModuleSourcesTest {

  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  private File f;

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
    var ensoContext = ctxRule.ensoContext();
    var tFile = ensoContext.getTruffleFile(f);
    var sourcesWithFile = sources.newWith(tFile);
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("rope is null", sourcesWithFile.rope() == null);
  }

  @Test
  public void moduleSourcesWithRopePreservesFile() {
    var sources = ModuleSources.NONE;
    var ensoContext = ctxRule.ensoContext();
    var tFile = ensoContext.getTruffleFile(f);
    var rope = Rope$.MODULE$.apply("foo");
    var sourcesWithFile = sources.newWith(tFile).newWith(rope);
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("rope is non-null", sourcesWithFile.rope() == rope);
  }

  @Test
  public void modulesSourcesResetPreservesFile() {
    var sources = ModuleSources.NONE;
    var ensoContext = ctxRule.ensoContext();
    var tFile = ensoContext.getTruffleFile(f);
    var rope = Rope$.MODULE$.apply("foo");
    var sourcesWithFile = sources.newWith(tFile).newWith(rope).reset();
    assertTrue("getPath is non-null", sourcesWithFile.getPath() == tFile.getPath());
    assertTrue("getPath is null", sourcesWithFile.rope() == null);
  }
}
