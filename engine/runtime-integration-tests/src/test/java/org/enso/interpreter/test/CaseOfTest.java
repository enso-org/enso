package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;

import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public class CaseOfTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void caseOfBoolean() {
    doCaseOfBoolean(true, false);
  }

  @Test
  public void caseOfInteropBoolean() {
    var t = new WrappedPrimitive(true);
    var f = new WrappedPrimitive(false);
    doCaseOfBoolean(t, f);
  }

  @Test
  public void caseOfMultiValueBoolean() {
    var n = EnsoMultiValue.NewNode.getUncached();

    var bAndT =
        new Type[] {
          ctxRule.ensoContext().getBuiltins().bool().getType(),
          ctxRule.ensoContext().getBuiltins().number().getInteger()
        };
    var t = n.newValue(bAndT, 2, 0, new Object[] {true, 300});
    var f = n.newValue(bAndT, 2, 0, new Object[] {false, 200});
    doCaseOfBoolean(t, f);
  }

  private void doCaseOfBoolean(Object t, Object f) {
    var code =
        """
        from Standard.Base import True, False

        choose v = case v of
            True -> 1
            False -> 2
            _ -> 3
        """;

    var choose = ctxRule.evalModule(code, "choose.enso", "choose");

    var one = choose.execute(t);
    assertEquals("With " + t + " we should get 1", 1, one.asInt());
    var two = choose.execute(f);
    assertEquals("With " + f + " we should get 2", 2, two.asInt());
  }

  /** See <a href="https://github.com/enso-org/enso/issues/14426">#14426</a>. */
  @Test
  public void caseOfJavaFinalFields_SmallInteger() {
    var code =
        """
        polyglot java import org.enso.example.TestConstants

        main =
            x = 1
            case x of
                TestConstants.A -> "A"
                _ -> "Unknown"
        """;
    var res = ctxRule.evalModule(code);
    assertThat(res.isString(), is(true));
    assertThat(res.asString(), is("A"));
  }

  /** See <a href="https://github.com/enso-org/enso/issues/14426">#14426</a>. */
  @Test
  public void caseOfJavaFinalFields_BiggerInteger() {
    var code =
        """
        polyglot java import org.enso.example.TestConstants

        main =
            x = 2020
            case x of
                TestConstants.B -> "B"
                _ -> "Unknown"
        """;
    var res = ctxRule.evalModule(code);
    assertThat(res.isString(), is(true));
    assertThat(res.asString(), is("B"));
  }
}
