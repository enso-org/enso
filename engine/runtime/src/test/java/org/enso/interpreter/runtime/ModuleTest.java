package org.enso.interpreter.runtime;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ModuleTest {

  private File f;
  private Context ctx;

  @Before
  public void prepareTest() throws IOException {

    f = File.createTempFile("module-sources", ".enso");
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
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
  public void moduleKeepsFileRefAfterSourceUnset() {
    var name = QualifiedName.simpleName("local.Unnamed_1");
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);
    var module = new Module(name, null, tFile);
    assertTrue(
        "getPath is non-null", tFile.getPath() != null && module.getPath() == tFile.getPath());
    module.unsetLiteralSource();
    assertTrue(
        "getPath is non-null", tFile.getPath() != null && module.getPath() == tFile.getPath());
  }
}
