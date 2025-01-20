package org.enso.interpreter.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class EqualsConversionsTest {
  private static Context context;

  @BeforeClass
  public static void initContextAndData() {
    context = ContextUtils.createDefaultContext();
  }

  @AfterClass
  public static void disposeContext() {
    context.close();
    context = null;
  }

  @Test
  public void testBasicInequalities() {
    var results =
        ContextUtils.evalModule(
                context,
                """
    from Standard.Base import all

    Text.from (that:Number) = that.to_text

    main =
        r0 = "4"+"2" == "42"
        r1 = 42 == "42"
        r2 = "42" == 42
        [ r0, r1, r2 ]
    """)
            .as(List.class);

    assertTrue("strings are equal: " + results, (boolean) results.get(0));
    assertFalse("string is not equal to number: " + results, (boolean) results.get(1));
    assertFalse("number is not equal to string: " + results, (boolean) results.get(2));
  }

  @Test
  public void testNumWrapperAroundIntegerIsEqualToInteger() {
    var gen = new DefineComparableWrapper();
    gen.intNumConversion = true;
    gen.intComparator = true;
    gen.numComparator = true;
    assertTrue("Num.Value equal to Integer: ", gen.evaluate());
  }

  @Test
  public void testMissingIntegerNumConversion() {
    var gen = new DefineComparableWrapper();
    gen.intNumConversion = false;
    gen.intComparator = true;
    gen.numComparator = true;
    assertFalse("Num.Value not equal to Integer: ", gen.evaluate());
  }

  @Test
  public void testMissingIntComparator() {
    var gen = new DefineComparableWrapper();
    gen.intNumConversion = true;
    gen.intComparator = false;
    gen.numComparator = true;
    assertFalse("Num.Value not equal to Integer: ", gen.evaluate());
  }

  @Test
  public void testMissingNumComparator() {
    var gen = new DefineComparableWrapper();
    gen.intNumConversion = true;
    gen.intComparator = true;
    gen.numComparator = false;
    assertFalse("Num.Value not equal to Integer: ", gen.evaluate());
  }

  @Test
  public void testDifferentComparators() {
    var gen = new DefineComparableWrapper();
    gen.intNumConversion = true;
    gen.intComparator = true;
    gen.numComparator = false;
    gen.extraBlock =
        """
    type Second_Comparator
        compare a:Num b:Num = Num_Comparator.compare a b
        hash a:Num = Num_Comparator.hash a

    Comparable.from (that:Num) = Comparable.new that Second_Comparator
    """;
    assertFalse("Num.Value not equal to Integer: ", gen.evaluate());
  }

  @Test
  public void testInconsistentHashFunction() {
    var gen = new DefineComparableWrapper();
    gen.intNumConversion = true;
    gen.intComparator = true;
    gen.numComparator = true;
    gen.hashFn = "x.n*31";

    boolean r;
    try {
      r = gen.evaluate();
    } catch (PolyglotException ex) {
      assertTrue(ex.getMessage(), ex.getMessage().contains("Different hash code!"));
      return;
    }
    fail("Expecting assertion error, not: " + r);
  }

  private static final class DefineComparableWrapper {
    boolean intNumConversion;
    boolean numComparator;
    boolean intComparator;
    String hashFn = "Ordering.hash x.n";
    String extraBlock = "";

    boolean evaluate() {
      var block0 =
          """
          from Standard.Base import all

          type Num
              Value n:Integer

          type Num_Comparator
              compare x:Num y:Num = Ordering.compare x.n y.n
              hash x:Num = {hashFn}
          """
              .replace("{hashFn}", hashFn);

      var block1 =
          !intNumConversion
              ? ""
              : """
          Num.from (that:Integer) = Num.Value that
          """;

      var block2 =
          !numComparator
              ? ""
              : """
          Comparable.from (that:Num) = Comparable.new that Num_Comparator
          """;

      var block3 =
          !intComparator
              ? ""
              : """
      Comparable.from (that:Integer) = Comparable.new that Num_Comparator
      """;

      var mainBlock =
          """
      main =
          num42 = Num.Value 42

          r0 = 42 == num42
          r0
      """;
      var res =
          ContextUtils.evalModule(
              context, block0 + block1 + block2 + block3 + mainBlock + extraBlock);
      return res.asBoolean();
    }
  }
}
