package org.enso.interpreter.test.semantic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Test;

public class FunctionIdentityTest {

  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void functionWithArgIdentity() throws Exception {
    var rawCode =
        """
        am_i_me _ = am_i_me...
        """;
    assertFunctionIdentity(rawCode, "Am_I_Me_With_Arg");
  }

  @Test
  public void functionIdentity() throws Exception {
    var rawCode =
        """
        am_i_me = am_i_me...
        """;
    assertFunctionIdentity(rawCode, "Am_I_Me");
  }

  private void assertFunctionIdentity(String code, String moduleName) throws Exception {
    var src = Source.newBuilder("enso", code, moduleName + ".enso").build();
    var module = ctxRule.eval(src);
    var fn = module.invokeMember("eval_expression", "am_i_me").execute(0);

    assertTrue("fn: " + fn, fn.canExecute());

    var rawFn = ctxRule.unwrapValue(fn);
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
