package org.enso.interpreter.test;

import static org.enso.test.utils.ContextUtils.createDefaultContext;
import static org.enso.test.utils.ContextUtils.executeInContext;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.common.MethodNames;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.foreign.HostValueToEnsoNode;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class EqualsMultiValueTest {
  private static Context context;
  private static EqualsNode equalsNode;
  private static TestRootNode testRootNode;
  private static HostValueToEnsoNode hostValueToEnsoNode;

  @BeforeClass
  public static void initContextAndData() {
    context = createDefaultContext();
    executeInContext(
        context,
        () -> {
          testRootNode = new TestRootNode(EqualsMultiValueTest::equalityCheck);
          equalsNode = EqualsNode.create();
          hostValueToEnsoNode = HostValueToEnsoNode.build();
          testRootNode.insertChildren(equalsNode, hostValueToEnsoNode);
          return null;
        });
  }

  @AfterClass
  public static void disposeContext() {
    context.close();
    context = null;
  }

  @Test
  public void testEqualityIntegerAndMultiValue() {
    executeInContext(
        context,
        () -> {
          var builtins = ContextUtils.leakContext(context).getBuiltins();
          var intType = builtins.number().getInteger();
          var textText = builtins.text();
          var fourExtraText =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(
                      new Type[] {intType, textText}, 1, 0, new Object[] {4L, Text.create("Hi")});

          assertTrue("4 == 4t", equalityCheck(4L, fourExtraText));
          assertFalse("5 != 4t", equalityCheck(5L, fourExtraText));
          assertTrue("4t == 4", equalityCheck(fourExtraText, 4L));
          assertFalse("4t != 5", equalityCheck(fourExtraText, 5L));

          return null;
        });
  }

  @Test
  public void testEqualityTextAndExtraIntegerMultiValue() {
    executeInContext(
        context,
        () -> {
          var builtins = ContextUtils.leakContext(context).getBuiltins();
          var intType = builtins.number().getInteger();
          var textType = builtins.text();
          var bothTypes = new Type[] {textType, intType};

          var text = Text.create("Hi");
          var ahoj = Text.create("Ahoj");
          var integer = 4L;
          //
          // following variable represents result of
          //     x = _ : (Text & Integer) : Text
          // e.g. multi value with Text and Integer, casted to Text only
          //
          var multiV =
              EnsoMultiValue.NewNode.getUncached().newValue(bothTypes, 1, 0, text, integer);

          assertTrue("'Hi' == multiV", equalityCheck(text, multiV));
          assertFalse("'Ahoj' != multiV", equalityCheck(ahoj, multiV));
          assertFalse(
              "Don't consider extra Integer type in equals", equalityCheck(integer, multiV));
          assertFalse("5 != t4", equalityCheck(5L, multiV));
          assertFalse(
              "Don't consider extra Integer type in equals", equalityCheck(multiV, integer));
          assertFalse("4 != t5", equalityCheck(multiV, 5L));
          assertTrue("multiV == 'Hi'", equalityCheck(multiV, text));
          assertFalse("multiV != 'Ahoj'", equalityCheck(multiV, ahoj));

          return null;
        });
  }

  @Test
  public void testEqualityIntegerAndMultiValueWithBoth() {
    executeInContext(
        context,
        () -> {
          var builtins = ContextUtils.leakContext(context).getBuiltins();
          var intType = builtins.number().getInteger();
          var textText = builtins.text();
          var hi = Text.create("Hi");
          var textFour =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(new Type[] {textText, intType}, 2, 0, new Object[] {hi, 4L});
          var textFive =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(new Type[] {textText, intType}, 2, 0, new Object[] {hi, 5L});
          var fourText =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(new Type[] {intType, textText}, 2, 0, new Object[] {4L, hi});

          assertFalse("4 != t", equalityCheck(4L, hi));
          assertFalse("4 != 4t", equalityCheck(4L, textFour));
          assertFalse("5 != 4t", equalityCheck(5L, textFour));
          assertFalse("5t != 4t", equalityCheck(textFive, textFour));
          assertFalse("4t != 4", equalityCheck(textFour, 4L));
          assertFalse("4t != 5", equalityCheck(textFour, 5L));
          assertFalse("4t != 'Hi'", equalityCheck(textFour, hi));
          assertFalse("'Hi' != 4t", equalityCheck(hi, textFour));

          assertTrue("t4 == 4t", equalityCheck(textFour, fourText));
          assertTrue("4t == t4", equalityCheck(fourText, textFour));

          return null;
        });
  }

  @Test
  public void testEqualityIntegerAndMultiValueWithIntText() {
    executeInContext(
        context,
        () -> {
          var builtins = ContextUtils.leakContext(context).getBuiltins();
          var intType = builtins.number().getInteger();
          var textText = builtins.text();
          var fourExtraText =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(
                      new Type[] {intType, textText}, 2, 0, new Object[] {4L, Text.create("Hi")});

          assertFalse("4 != 4t", equalityCheck(4L, fourExtraText));
          assertFalse("5 != 4t", equalityCheck(5L, fourExtraText));
          assertFalse("4t != 4", equalityCheck(fourExtraText, 4L));
          assertFalse("4t != 5", equalityCheck(fourExtraText, 5L));

          return null;
        });
  }

  @Test
  public void twoMultiValues() {
    executeInContext(
        context,
        () -> {
          var builtins = ContextUtils.leakContext(context).getBuiltins();
          var intType = builtins.number().getInteger();
          var textText = builtins.text();
          var fourExtraText =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(
                      new Type[] {intType, textText}, 1, 0, new Object[] {4L, Text.create("Hi")});
          var fourExtraText2 =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(
                      new Type[] {intType, textText}, 1, 0, new Object[] {4L, Text.create("Hi")});
          var fiveExtraText =
              EnsoMultiValue.NewNode.getUncached()
                  .newValue(
                      new Type[] {intType, textText}, 1, 0, new Object[] {5L, Text.create("Hi")});

          assertFalse("!= for sure #1", equalityCheck(fiveExtraText, fourExtraText));
          assertFalse("!= for sure #2", equalityCheck(fourExtraText, fiveExtraText));
          assertTrue("equals #1", equalityCheck(fourExtraText, fourExtraText2));
          assertTrue("equals #2", equalityCheck(fourExtraText2, fourExtraText));

          return null;
        });
  }

  @Test
  public void testEqualityIntegerNoMultiValueWithConversion() throws Exception {
    assertEqualityIntegerWithConversion("c:Complex");
  }

  @Test
  public void testEqualityIntegerAndMultiValueWithConversion() throws Exception {
    assertEqualityIntegerWithConversion("c.as_complex_and_float");
  }

  private void assertEqualityIntegerWithConversion(String complexNew) throws Exception {
    var code =
        """
    import Standard.Base.Data.Numbers.Float
    import Standard.Base.Data.Numbers.Number
    import Standard.Base.Data.Ordering.Comparable
    import Standard.Base.Data.Ordering.Ordering
    import Standard.Base.Nothing
    import Standard.Base.Error.Error
    import Standard.Base.Errors.Illegal_Argument.Illegal_Argument

    ## Sample definition of a complex number with conversions
      from Number and implementation of a comparator.
    type Complex
        private Value re:Float im:Float

        new re=0:Float im=0:Float -> Complex =
            c = Complex.Value re 0
            if im != 0 then c:Complex else
                ${complexNew}

        + self (that:Complex) = Complex.new self.re+that.re self.im+that.im

        < self (that:Complex) = Complex_Comparator.compare self that == Ordering.Less
        > self (that:Complex) = Complex_Comparator.compare self that == Ordering.Greater
        >= self (that:Complex) =
            ordering = Complex_Comparator.compare self that
            ordering == Ordering.Greater || ordering == Ordering.Equal
        <= self (that:Complex) =
            ordering = Complex_Comparator.compare self that
            ordering == Ordering.Less || ordering == Ordering.Equal

    Complex.from (that:Number) = Complex.new that


    Comparable.from (that:Complex) = Comparable.new that Complex_Comparator
    Comparable.from (that:Number) = Comparable.new that Complex_Comparator

    type Complex_Comparator
        compare x:Complex y:Complex = if x.re==y.re && x.im==y.im then Ordering.Equal else
            if x.im==0 && y.im==0 then Ordering.compare x.re y.re else
                Nothing
        hash x:Complex = if x.im == 0 then Ordering.hash x.re else
            7*x.re + 11*x.im

    ## uses the explicit conversion defined in this private module
    Complex.as_complex_and_float self =
        self : Complex&Float

    ## explicit "conversion" of `Complex` to `Float` in a private module
       used in `as_complex_and_float`
    Float.from (that:Complex) =
        if that.im == 0 then that.re else
            Error.throw <| Illegal_Argument.Error "Cannot convert Complex with imaginary part to Float"
    """
            .replace("${complexNew}", complexNew);

    var src = Source.newBuilder("enso", code, "complex.enso").build();
    var complexModule = context.eval(src);
    var complexFourValue =
        complexModule.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "Complex.new 4");

    executeInContext(
        context,
        () -> {
          var complexFour = ContextUtils.unwrapValue(context, complexFourValue);

          assertTrue("4 == 4t", equalityCheck(4L, complexFour));
          assertFalse("5 != 4t", equalityCheck(5L, complexFour));
          assertTrue("4t == 4", equalityCheck(complexFour, 4L));
          assertFalse("4t != 5", equalityCheck(complexFour, 5L));

          return null;
        });
  }

  private static boolean equalityCheck(VirtualFrame frame) {
    var args = frame.getArguments();
    return equalsNode.execute(frame, args[0], args[1]).isTrue();
  }

  private boolean equalityCheck(Object first, Object second) {
    return (Boolean) testRootNode.getCallTarget().call(first, second);
  }
}
