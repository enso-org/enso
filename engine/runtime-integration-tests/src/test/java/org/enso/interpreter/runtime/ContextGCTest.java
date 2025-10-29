package org.enso.interpreter.runtime;

import static org.junit.Assert.assertEquals;

import org.enso.test.utils.ContextUtils;
import org.junit.Rule;
import org.junit.Test;

public class ContextGCTest {
  @Rule public final ContextUtils ctx = ContextUtils.createDefault();

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
}
