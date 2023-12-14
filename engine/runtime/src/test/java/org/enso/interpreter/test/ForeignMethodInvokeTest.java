package org.enso.interpreter.test;

import static org.junit.Assert.assertTrue;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ForeignMethodInvokeTest extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void prepareCtx() {
    ctx = defaultContextBuilder("enso").build();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
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
