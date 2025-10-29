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
