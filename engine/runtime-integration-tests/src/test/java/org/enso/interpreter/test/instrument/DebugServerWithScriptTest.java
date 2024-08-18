package org.enso.interpreter.test.instrument;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import org.enso.polyglot.debugger.DebugServerInfo;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.hamcrest.core.AllOf;
import org.junit.Test;

public class DebugServerWithScriptTest {
  private interface WithContext {
    void action(Context ctx, Object out, Object err) throws Exception;
  }

  private void withContext(WithContext action) throws Exception {
    var out = new ByteArrayOutputStream();
    var err = new ByteArrayOutputStream();

    var b = ContextUtils.defaultContextBuilder().out(out).err(err);

    b.option(DebugServerInfo.FN_OPTION, "ScriptTest.inspect");

    try (var ctx = b.build()) {
      action.action(ctx, out, err);
    }
  }

  @Test
  public void propertyListingVariables() throws Exception {
    withContext(
        (ctx, out, err) -> {
          var r =
              ContextUtils.evalModule(
                  ctx,
                  """
            from Standard.Base import all

            inspect =
                j = 1
                d = Warning.attach "doubled value" 2
                t = j + d
                v = [j, d, t]
                v
            """,
                  "ScriptTest",
                  "inspect");
          assertTrue("Got array back: " + r, r.hasArrayElements());
          assertEquals("Got three elements", 3, r.getArraySize());
          assertEquals("One", 1, r.getArrayElement(0).asInt());
          assertEquals("Two", 2, r.getArrayElement(1).asInt());
          assertEquals("Three", 3, r.getArrayElement(2).asInt());
          assertEquals("No output printed", "", out.toString());
          assertThat(
              "Error contains some warnings",
              err.toString(),
              AllOf.allOf(
                  containsString("d = 2"),
                  containsString("t = 3"),
                  containsString("doubled value"),
                  not(containsString("j = 1"))));
        });
  }
}
