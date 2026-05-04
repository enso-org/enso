package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class BinaryTypeCheckTest {
  @ClassRule
  public static final ContextUtils ctxRule = ContextUtils.newBuilder().assertGC(false).build();

  private static Value module;

  @BeforeClass
  public static void initCtx() throws Exception {
    var prelude =
        Source.newBuilder(
                "enso",
                """
                from Standard.Base import all

                type Truth
                    Yes
                    No

                    &&& self other:Truth -> Truth =
                        case self of
                            Truth.No -> Truth.No
                            _ -> other

                    %$% self ~other:Truth -> Truth =
                        case self of
                            Truth.No -> Truth.No
                            _ -> other

                Truth.formulate self yes no = case self of
                    Truth.Yes -> yes
                    Truth.No -> no

                good_or_bad_1 =
                  (Truth.Yes &&& Truth.No) . formulate "Good" "Bad"

                good_or_bad_2 =
                  (Truth.Yes %$% Truth.No) . formulate "Good" "Bad"

                crash_3 =
                  Truth.Yes &&& Truth.No . formulate "Good" "Bad"

                crash_4 =
                  Truth.Yes %$% Truth.No . formulate "Good" "Bad"
                """,
                "truth.enso")
            .build();
    module = ctxRule.eval(prelude);
  }

  @AfterClass
  public static void closeCtx() {
    module = null;
  }

  @Test
  public void goodOrBad1() {
    var bad = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "good_or_bad_1");
    assertTrue(bad.isString());
    assertEquals("Bad", bad.asString());
  }

  @Test
  public void goodOrBad2() {
    var bad = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "good_or_bad_2");
    assertTrue(bad.isString());
    assertEquals("Bad", bad.asString());
  }

  @Test
  public void crash3() {
    try {
      var nothing = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "crash_3");
      fail("Expecting exception, but got: " + nothing);
    } catch (PolyglotException ex) {
      assertEquals("Type error: expected `other` to be Truth, but got Text.", ex.getMessage());
    }
  }

  @Test
  public void crash4() {
    try {
      var nothing = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "crash_4");
      fail("Expecting exception, but got: " + nothing);
    } catch (PolyglotException ex) {
      assertEquals("Type error: expected `other` to be Truth, but got Text.", ex.getMessage());
    }
  }
}
