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
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class DebugServerWithScriptTest {
  private static Context ctx;
  private static ByteArrayOutputStream out = new ByteArrayOutputStream();
  private static ByteArrayOutputStream err = new ByteArrayOutputStream();

  @BeforeClass
  public static void initContext() throws Exception {
    var b = ContextUtils.defaultContextBuilder().out(out).err(err);
    b.option(DebugServerInfo.FN_OPTION, "ScriptTest.inspect");
    ctx = b.build();
  }

  @AfterClass
  public static void closeContext() throws Exception {
    ctx.close();
    ctx = null;
    out = null;
    err = null;
  }

  @Before
  public void cleanSteams() {
    out.reset();
    err.reset();
  }

  @Test
  public void listingVariablesWithWarnings() throws Exception {
    var code =
        """
        from Standard.Base import all

        inspect =
            j = 1
            d = Warning.attach "doubled value" 2
            t = j + d
            v = [j, d, t]
            v
        """;
    var r = ContextUtils.evalModule(ctx, code, "ScriptTest.enso", "inspect");
    assertTrue("Got array back: " + r, r.hasArrayElements());
    assertEquals("Got three elements", 3, r.getArraySize());
    assertEquals("One", 1, r.getArrayElement(0).asInt());
    assertEquals("Two", 2, r.getArrayElement(1).asInt());
    assertEquals("Three", 3, r.getArrayElement(2).asInt());
    assertEquals("No output printed", "", out.toString());
    assertThat(
        "Stderr contains some warnings",
        err.toString(),
        AllOf.allOf(
            containsString("d = 2"),
            containsString("t = 3"),
            containsString("doubled value"),
            not(containsString("j = 1"))));
  }

  @Test
  public void panicOnError() throws Exception {
    var code =
        """
        from Standard.Base import all

        inspect =
            j = 1
            d = Error.throw 2
            t = j + d
            v = [j, d, t]
            v
        """;
    var r = ContextUtils.evalModule(ctx, code, "ScriptTest.enso", "inspect");
    assertTrue("Got error back: " + r, r.isException());
    assertEquals("(Error: 2)", r.toString());
    assertEquals("No output printed", "", out.toString());
    assertThat(
        "Stderr contains some errors",
        err.toString(),
        AllOf.allOf(
            containsString("d = Error:2"),
            containsString("t = Error:2"),
            not(containsString("j = 1"))));
  }
}
