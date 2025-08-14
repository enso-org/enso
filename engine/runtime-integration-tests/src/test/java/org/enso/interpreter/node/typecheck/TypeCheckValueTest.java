package org.enso.interpreter.node.typecheck;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.CallTarget;
import org.enso.compiler.core.ir.AscriptionReason;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.warning.Warning;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
import org.enso.interpreter.runtime.warning.WithWarnings;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.junit.ClassRule;
import org.junit.Test;

public class TypeCheckValueTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void avoidDoubleWrappingOfEnsoMultiValue() {
    var convert = allOfIntegerAndText();

    var builtins = ctxRule.ensoContext().getBuiltins();
    var hi = Text.create("Hi");
    var m1 =
        EnsoMultiValue.NewNode.getUncached()
            .newValue(new Type[] {builtins.text(), builtins.number().getInteger()}, 2, 0, hi, 42);
    assertEquals("'Hi'", m1.toDisplayString(true));

    var res = convert.call(m1);
    assertTrue("Got multivalue again", res instanceof EnsoMultiValue);
    var emv = (EnsoMultiValue) res;

    assertEquals("42", emv.toDisplayString(true));
  }

  private static CallTarget allOfIntegerAndText() {
    var call = new CallTarget[1];
    var builtins = ctxRule.ensoContext().getBuiltins();
    var intNode =
        TypeCheckValueNode.single(
            AscriptionReason.forParameter("int"), builtins.number().getInteger());
    var textNode =
        TypeCheckValueNode.single(AscriptionReason.forParameter("text"), builtins.text());
    var bothNode =
        TypeCheckValueNode.allOf(AscriptionReason.forParameter("int&text"), intNode, textNode);
    var root =
        new TestRootNode(
            (frame) -> {
              var arg = frame.getArguments()[0];
              var res = bothNode.handleCheckOrConversion(frame, arg);
              return res;
            });
    root.insertChildren(bothNode);
    call[0] = root.getCallTarget();
    return call[0];
  }

  @Test
  public void checkWithWarnings() throws Exception {
    var ctx = ctxRule.ensoContext();
    var builtins = ctx.getBuiltins();

    var convert = allOfIntegerAndText();
    var hi = Text.create("Hi");
    var m1 =
        EnsoMultiValue.NewNode.getUncached()
            .newValue(new Type[] {builtins.text(), builtins.number().getInteger()}, 2, 0, hi, 42);
    var warning = Warning.create(ctx, Text.create("Problem"), builtins.nothing());
    var w1 = WithWarnings.create(m1, 100, false, warning);

    assertEquals("'Hi'", w1.toDisplayString(true).toString());

    var with = convert.call(w1);
    assertTrue("Converted value has warnings", WarningsLibrary.getUncached().hasWarnings(with));

    var res = WarningsLibrary.getUncached().removeWarnings(with);

    assertTrue("Got multivalue again", res instanceof EnsoMultiValue);
    var emv = (EnsoMultiValue) res;

    assertEquals("42", emv.toDisplayString(true));
  }
}
