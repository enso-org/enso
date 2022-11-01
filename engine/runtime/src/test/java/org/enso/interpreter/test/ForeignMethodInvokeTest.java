package org.enso.interpreter.test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeNotNull;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.Map;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ForeignMethodInvokeTest {
  private Context ctx;

  @Before
  public void prepareCtx() {
    Engine eng = Engine.newBuilder()
        .allowExperimentalOptions(true)
        .logHandler(new ByteArrayOutputStream())
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build();
    this.ctx = Context.newBuilder("enso")
        .engine(eng)
        .allowIO(true)
        .allowAllAccess(true)
        .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assumeNotNull("Enso not found: " + langs, langs.get("enso"));
  }

  @After
  public void disposeCtx() {
    this.ctx.close();
  }

  @Test
  public void testForeignFunctionParseFailure() {
    // python is not a permitted language, therefore, invoking `py_array` method
    // should fail with a Polyglot_Error, rather than crashing whole engine.
    String source = """
        from Standard.Base import all
        
        foreign python py_array = \"\"\"
            return [1,2,3]
        
        main =
            Panic.recover Any py_array
        """.trim();
    Value module = ctx.eval("enso", source);
    Value res = module.invokeMember("eval_expression", "main");
    assertTrue("Invoking non-installed foreign function should recover", res.isException());
    try {
      throw res.throwException();
    } catch (Exception e) {
      assertTrue("Wrong error message",
          e.getMessage().matches("Cannot parse foreign python method. Only available languages are .+"));
    }
  }
}
