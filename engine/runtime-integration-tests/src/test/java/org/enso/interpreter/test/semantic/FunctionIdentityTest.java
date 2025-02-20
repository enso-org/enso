package org.enso.interpreter.test.semantic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.nio.file.Paths;
import java.util.Map;
import java.util.logging.Level;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class FunctionIdentityTest {

  private Context ctx;

  @Before
  public void initContext() {
    ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .build();

    var engine = ctx.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @After
  public void disposeContext() {
    ctx.close();
    ctx = null;
  }

  @Test
  public void functionWithArgIdentity() throws Exception {
    var rawCode = """
        am_i_me _ = am_i_me...
        """;
    assertFunctionIdentity(rawCode, "Am_I_Me_With_Arg");
  }

  @Test
  public void functionIdentity() throws Exception {
    var rawCode = """
        am_i_me = am_i_me...
        """;
    assertFunctionIdentity(rawCode, "Am_I_Me");
  }

  private void assertFunctionIdentity(String code, String moduleName) throws Exception {
    var src = Source.newBuilder("enso", code, moduleName + ".enso").build();
    var module = ctx.eval(src);
    var fn = module.invokeMember("eval_expression", "am_i_me").execute(0);

    assertTrue("fn: " + fn, fn.canExecute());

    var rawFn = ContextUtils.unwrapValue(ctx, fn);
    assertTrue("is Function: " + rawFn, rawFn instanceof Function);

    var iop = InteropLibrary.getUncached();
    assertEquals(moduleName + ".am_i_me", iop.getExecutableName(rawFn));
    assertTrue("Has location", iop.hasSourceLocation(rawFn));
    var loc = iop.getSourceLocation(rawFn);
    assertNotNull("Location found", loc);
    assertEquals(
        "am_i_me function definition is on the first line",
        code.split("\n")[0],
        loc.getCharacters().toString());
  }
}
