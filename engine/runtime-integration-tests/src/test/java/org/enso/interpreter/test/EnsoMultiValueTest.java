package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.List;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.TypeLiteral;
import org.junit.AfterClass;
import org.junit.Test;

public class EnsoMultiValueTest {
  private static Context ctx;

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.defaultContextBuilder().build();
    }
    return ctx;
  }

  @AfterClass
  public static void disposeCtx() throws Exception {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
  }

  @Test
  public void trippleCastConfusion() {
    var code =
        """
    type A
        A_Ctor x
    type B
        B_Ctor x
    type C
        C_Ctor x

    B.from (that : A) = B.B_Ctor that
    C.from (that:B) = C.C_Ctor that

    texts =
        a = A.A_Ctor 1
        ab = (a : A & B)
        abc = ab:(A & B & C)
        c = abc:C

        text_a = (c:A).to_text
        text_b = (c:B).to_text
        [text_a, text_b, c.to_text]
    """;

    var tripple = ContextUtils.evalModule(ctx(), code, "tripple.enso", "texts");
    var texts = tripple.as(new TypeLiteral<List<String>>() {});
    assertEquals(3, texts.size());
    assertStartsWith("(A_Ctor", texts.get(0));
    assertStartsWith("(B_Ctor", texts.get(1));
    assertStartsWith("(C_Ctor", texts.get(2));
  }

  private static void assertStartsWith(String exp, String actual) {
    if (actual.startsWith(exp)) {
      return;
    }
    fail("Expecting " + exp + " in " + actual);
  }
}
