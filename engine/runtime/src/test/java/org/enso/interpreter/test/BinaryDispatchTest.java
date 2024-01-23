package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.polyglot.MethodNames;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class BinaryDispatchTest extends TestBase {
  private static Context ctx;
  private static Value module;

  public BinaryDispatchTest() {}

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = createDefaultContext();

    var prelude =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Data.Numbers

                type Z
                    Number n

                    --- self that:Z = (self.n.abs - that.n.abs).abs

                type R
                    Fraction r q

                    --- self that:R =
                        s = self.r/self.q
                        t = that.r/that.q
                        (s.abs - t.abs).abs

                R.from (that:Z) = R.Fraction that.n 1

                """,
                "prelude.enso")
            .build();
    module = ctx.eval(prelude);
  }

  @AfterClass
  public static void closeCtx() {
    ctx.close();
  }

  @Test
  public void verifyZ() {
    var six = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 6");
    var ten = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 10");

    var diff1 = six.invokeMember("---", ten);
    assertEquals(diff1.asInt(), 4);
  }

  @Test
  public void verifyR() {
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var oneEight = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 8");

    var diff1 = half.invokeMember("---", oneEight);
    assertEquals(diff1.asDouble(), 3.0 / 8, 0.01);
  }

  @Test
  public void thatArgumentIsConverted() {
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var two = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");

    var diff1 = half.invokeMember("---", two);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void selfArgumentIsConverted() {
    var one = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 1");
    var threeHalfs = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 3 2");

    var diff1 = one.invokeMember("---", threeHalfs);
    assertEquals(diff1.asDouble(), 1.0 / 2, 0.01);
  }

  @Test
  public void staticVerifyZ() {
    var zOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.---");
    assertTrue("It's executable", zOperator.canExecute());

    var six = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 6");
    var ten = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 10");

    var diff1 = zOperator.execute(six, ten);
    assertEquals(diff1.asInt(), 4);
  }

  @Test
  public void staticVerifyR() {
    var rOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.---");
    assertTrue("It's executable", rOperator.canExecute());

    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var oneEight = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 8");

    var diff1 = rOperator.execute(half, oneEight);
    assertEquals(diff1.asDouble(), 3.0 / 8, 0.01);
  }

  @Test
  @Ignore // PENDING: #8805
  public void staticWithRFirstArgumentIsConverted() {
    var rOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.---");

    var two = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");

    var diff1 = rOperator.execute(two, half);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void staticWithRSecondArgumentIsConverted() {
    var rOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.---");

    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var two = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");

    var diff1 = rOperator.execute(half, two);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void staticWithZFirstAndRSecondNoConversionHappens() {
    var zOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.---");

    var two = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");

    try {
      var diff1 = zOperator.execute(two, half);
      fail("Shouldn't return a value: " + diff1);
    } catch (PolyglotException ex) {
      assertContains("Type error", ex.getMessage());
      assertContains("`that` to be Z", ex.getMessage());
    }
  }

  @Test
  @Ignore // PENDING #8805
  public void staticWithRFirstAndZSecondNoConversionHappens() {
    var zOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.---");

    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var two = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");

    try {
      var diff1 = zOperator.execute(half, two);
      fail("Shouldn't return a value: " + diff1);
    } catch (PolyglotException ex) {
      assertContains("Type error", ex.getMessage());
      assertContains("`self` to be Z", ex.getMessage());
    }
  }

  private static void assertContains(String expected, String actual) {
    if (actual.contains(expected)) {
      return;
    }
    fail("Expecting " + expected + " in " + actual);
  }
}
