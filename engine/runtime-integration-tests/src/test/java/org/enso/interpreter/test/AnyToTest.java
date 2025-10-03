package org.enso.interpreter.test;

import static org.junit.Assert.assertTrue;

import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;

public class AnyToTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void multiValueToInteger() throws Exception {
    var ensoCtx = ctxRule.ensoContext();
    var types =
        new Type[] {ensoCtx.getBuiltins().number().getInteger(), ensoCtx.getBuiltins().text()};
    var code =
        """
        from Standard.Base import all

        private eq a b = a == b

        conv style v = case style of
            0 -> v.to Integer
            1 -> v:Integer
            99 -> eq

        """;
    var conv = ctxRule.evalModule(Source.newBuilder("enso", code, "conv.enso").build(), "conv");
    var both =
        EnsoMultiValue.NewNode.getUncached()
            .newValue(types, types.length, 0, new Object[] {2L, Text.create("Two")});
    var bothValue = ctxRule.asValue(both);
    var asIntegerTo = conv.execute(0, bothValue);
    var asIntegerCast = conv.execute(1, bothValue);
    var equals = conv.execute(99, null);
    var eq = equals.execute(asIntegerTo, asIntegerCast);
    assertTrue("Any.to and : give the same result", eq.asBoolean());
  }

  @Test
  @Ignore
  public void multiValueToText() throws Exception {
    multiValueToText(2);
  }

  @Test
  @Ignore
  public void multiValueToTextHidden() throws Exception {
    multiValueToText(1);
  }

  private void multiValueToText(int dispatchLength) throws Exception {
    var ensoCtx = ctxRule.ensoContext();
    var types =
        new Type[] {ensoCtx.getBuiltins().number().getInteger(), ensoCtx.getBuiltins().text()};
    var code =
        """
        from Standard.Base import all

        private eq a b = a == b

        conv style:Integer v = case style of
            2 -> v.to Text
            3 -> v:Text
            99 -> eq

        """;
    var conv = ctxRule.evalModule(Source.newBuilder("enso", code, "conv.enso").build(), "conv");
    var both =
        EnsoMultiValue.NewNode.getUncached()
            .newValue(types, dispatchLength, 0, new Object[] {2L, Text.create("Two")});
    var bothValue = ctxRule.asValue(both);
    var asIntegerCast = conv.execute(3, bothValue);
    var asIntegerTo = conv.execute(2, bothValue);
    var equals = conv.execute(99, null);
    var eq = equals.execute(asIntegerTo, asIntegerCast);
    assertTrue("Any.to and : give the same result", eq.asBoolean());
  }
}
