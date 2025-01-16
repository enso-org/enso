package org.enso.interpreter.test;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class AnyToTest {
  private static Context ctx;

  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void initCtx() {
    ctx = ContextUtils.createDefaultContext(out);
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  @Before
  public void resetOutput() {
    out.reset();
  }

  private String getStdOut() {
    return out.toString(StandardCharsets.UTF_8);
  }

  @Test
  public void multiValueToInteger() throws Exception {
    var ensoCtx = ContextUtils.leakContext(ctx);
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
    var conv =
        ContextUtils.evalModule(ctx, Source.newBuilder("enso", code, "conv.enso").build(), "conv");
    var both =
        EnsoMultiValue.NewNode.getUncached()
            .newValue(types, types.length, 0, new Object[] {2L, Text.create("Two")});
    var eq =
        ContextUtils.executeInContext(
            ctx,
            () -> {
              var bothValue = ctx.asValue(both);
              var asIntegerTo = conv.execute(0, bothValue);
              var asIntegerCast = conv.execute(1, bothValue);
              var equals = conv.execute(99, null);
              return equals.execute(asIntegerTo, asIntegerCast);
            });
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
    var ensoCtx = ContextUtils.leakContext(ctx);
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
    var conv =
        ContextUtils.evalModule(ctx, Source.newBuilder("enso", code, "conv.enso").build(), "conv");
    var both =
        EnsoMultiValue.NewNode.getUncached()
            .newValue(types, dispatchLength, 0, new Object[] {2L, Text.create("Two")});
    var eq =
        ContextUtils.executeInContext(
            ctx,
            () -> {
              var bothValue = ctx.asValue(both);
              var asIntegerCast = conv.execute(3, bothValue);
              var asIntegerTo = conv.execute(2, bothValue);
              var equals = conv.execute(99, null);
              return equals.execute(asIntegerTo, asIntegerCast);
            });
    assertTrue("Any.to and : give the same result", eq.asBoolean());
  }
}
