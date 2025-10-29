package org.enso.interpreter.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import org.enso.test.utils.ContextUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ContextGCTest {
  private ContextUtils ctx;

  @Before
  public void initCtx() {
    ctx = ContextUtils.newBuilder().build();
  }

  @After
  public void closeCtxAndGC() throws Exception {
    var ref = new WeakReference<>(ctx);
    ctx.close();
    ctx = null;
    assertGC("Context has to GC", true, ref);
  }

  @Test
  public void simpleEval() throws Exception {
    var fourtyTwo =
        ctx.evalModule(
            """
            from Standard.Base import all

            main = 6 * 7
            """);
    assertEquals(42, fourtyTwo.asInt());
  }

  @Test
  public void multiValue() throws Exception {
    var arr =
        ctx.evalModule(
            """
            from Standard.Base import all

            type T

            Integer.from (_:T) = 42
            Text.from (_:T) = "Meaning"

            main =
                conv t -> Integer&Text = t
                v = conv T
                [v, v:Text, v:Integer]

            """);
    assertEquals(42, arr.getArrayElement(2).asInt());
    assertEquals("Meaning", arr.getArrayElement(1).asString());
  }

  private static void assertGC(String msg, boolean expectGC, Reference<?> ref) {
    for (var i = 1; i < Integer.MAX_VALUE / 2; i *= 2) {
      if (ref.get() == null) {
        break;
      }
      System.gc();
    }
    var obj = ref.get();
    if (expectGC) {
      assertNull(msg + " ref still alive", obj);
    } else {
      assertNotNull(msg + " ref has been cleaned", obj);
    }
  }
}
