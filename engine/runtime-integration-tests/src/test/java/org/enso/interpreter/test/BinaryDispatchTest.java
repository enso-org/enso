package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.common.MethodNames;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.hamcrest.core.AllOf;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class BinaryDispatchTest {
  private static Context ctx;
  private static Value module;

  public BinaryDispatchTest() {}

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = ContextUtils.createDefaultContext();

    var prelude =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Data.Numbers
                from Standard.Base import Text

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
                Text.from (that:Z) = "z["+that.n.to_text+"]"

                wrapRText (n : R & Text) = n
                wrapTextR (n : Text & R) = n
                wrapZText (n : Z & Text) = n
                wrapTextZ (n : Text & Z) = n
                """,
                "prelude.enso")
            .build();
    module = ctx.eval(prelude);
  }

  @AfterClass
  public static void closeCtx() {
    module = null;
    ctx.close();
    ctx = null;
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
  public void thatArgumentIsMultiRText() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapRText");
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");
    var two = wrap.execute(z);

    var diff1 = half.invokeMember("---", two);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void selfArgumentIsMultiRText() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapRText");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 1");
    var one = wrap.execute(z);
    var threeHalfs = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 3 2");

    var diff1 = one.invokeMember("---", threeHalfs);
    assertEquals(diff1.asDouble(), 1.0 / 2, 0.01);
  }

  @Test
  public void thatArgumentIsMultiTextR() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapTextR");
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");
    var two = wrap.execute(z);

    var diff1 = half.invokeMember("---", two);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void selfArgumentIsMultiTextR() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapTextR");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 1");
    var one = wrap.execute(z);
    var threeHalfs = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 3 2");

    var diff1 = one.invokeMember("---", threeHalfs);
    assertEquals(diff1.asDouble(), 1.0 / 2, 0.01);
  }

  @Test
  public void thatArgumentIsMultiZText() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapZText");
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");
    var two = wrap.execute(z);

    var diff1 = half.invokeMember("---", two);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void selfArgumentIsMultiZText() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapZText");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 1");
    var one = wrap.execute(z);
    var threeHalfs = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 3 2");

    var diff1 = one.invokeMember("---", threeHalfs);
    assertEquals(diff1.asDouble(), 1.0 / 2, 0.01);
  }

  @Test
  public void thatArgumentIsMultiTextZ() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapTextZ");
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");
    var two = wrap.execute(z);

    var diff1 = half.invokeMember("---", two);
    assertEquals(diff1.asDouble(), 3.0 / 2, 0.01);
  }

  @Test
  public void selfArgumentIsMultiTextZ() {
    var wrap = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "wrapTextZ");
    var z = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 1");
    var one = wrap.execute(z);
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
      assertThat(
          ex.getMessage(),
          AllOf.allOf(containsString("Type error"), containsString("`that` to be Z")));
    }
  }

  @Test
  public void staticWithRFirstAndZSecondNoConversionHappens() {
    var zOperator = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.---");

    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");
    var two = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Z.Number 2");

    try {
      var diff1 = zOperator.execute(half, two);
      fail("Shouldn't return a value: " + diff1);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(containsString("Type error"), containsString("`self` to be Z")));
    }
  }

  @Test
  public void thatArgumentChallengedByManyValues() throws Exception {
    var half = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "R.Fraction 1 2");

    var g = ValuesGenerator.create(ctx);

    for (var second : g.allValues()) {
      try {
        var result = half.invokeMember("---", second);
        if (result.isException()) {
          continue;
        }
        fail("No result expected: " + result + " for " + second);
      } catch (PolyglotException e) {
        if (e.getMessage().contains("expected `that` to be R, but got")) {
          continue;
        }
        throw e;
      } catch (PanicException e) {
        if (e.getMessage().contains("Type_Error")) {
          continue;
        }
        throw e;
      }
    }
  }
}
