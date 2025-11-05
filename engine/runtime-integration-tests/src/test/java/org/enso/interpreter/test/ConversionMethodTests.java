package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.stream.Stream;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;

public class ConversionMethodTests {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @After
  public void resetOutput() {
    ctx.resetOut();
  }

  private String getStdOut() {
    return ctx.getOut();
  }

  @Test
  public void testSimpleConversion() {
    String src =
        """
        type Foo
            Mk_Foo foo
        type Bar
            Mk_Bar bar
        type Baz
            Mk_Baz baz

        Foo.from (that:Bar) = Foo.Mk_Foo that.bar
        Foo.from (that:Baz) = Foo.Mk_Foo that.baz

        main = (Foo.from (Baz.Mk_Baz 10)).foo + (Foo.from (Bar.Mk_Bar 20)).foo
        """;
    Value res = ctx.evalModule(src);
    assertEquals(30, res.asInt());
  }

  @Test
  public void testDispatchOnHostMap() {
    String src =
        """
        polyglot java import java.util.Map as Java_Map
        import Standard.Base.Data.Dictionary.Dictionary

        type Foo
           Mk_Foo data

        Foo.from (that:Dictionary) = Foo.Mk_Foo that

        main =
            jmap = Java_Map.of "A" 1 "B" 2 "C" 3
            Foo.from jmap . data . size
        """;
    Value res = ctx.evalModule(src);
    assertEquals(3, res.asInt());
  }

  @Test
  public void testDispatchOnJSMap() {
    String src =
        """
        import Standard.Base.Data.Dictionary.Dictionary

        foreign js js_map = '''
            let m = new Map()
            m.set("A", 1)
            m.set("B", 2)
            return m

        type Foo
           Mk_Foo data

        Foo.from (that:Dictionary) = Foo.Mk_Foo that

        main =
            Foo.from js_map . data . size
        """;
    Value res = ctx.evalModule(src);
    assertEquals(2, res.asInt());
  }

  @Test
  public void testDispatchOnJSDateTime() {
    String src =
        """
        import Standard.Base.Data.Time.Date_Time.Date_Time

        foreign js js_date year month day hour minute second nanosecond = '''
            return new Date(year, month - 1, day, hour, minute, second, nanosecond / 1000000);

        type Foo
           Mk_Foo data

        Foo.from (that:Date_Time) = Foo.Mk_Foo that

        main =
           Foo.from (js_date 2023 2 7 23 59 0 10) . data . day
        """;
    Value res = ctx.evalModule(src);
    assertEquals(7, res.asInt());
  }

  @Test
  public void testAmbiguousConversionStrictUnused() {
    String src =
        """
        type Foo
           Mk_Foo data
        type Bar
           Mk_Bar x

        Foo.from (that:Bar) = Foo.Mk_Foo that.x+100
        Foo.from (that:Bar) = Foo.Mk_Foo that.x+1000

        main = 42
        """;
    try {
      Value res = ctx.evalModule(src);
      fail("Expected an exception, but got " + res);
    } catch (Exception e) {
      MatcherAssert.assertThat(e.getMessage(), Matchers.containsString("Ambiguous conversion:"));
      MatcherAssert.assertThat(
          getStdOut(),
          Matchers.containsString(
              "Unnamed:7:1: error: Ambiguous conversion: Foo.from Bar is defined multiple times in"
                  + " this module."));
    }
  }

  @Test
  public void testNoConversionWhenMultiValueMatches() {
    String src =
        """
        from Standard.Base import Any, Integer, Meta, Runtime

        type Foo
           F n
        type Bar
           B n

        Foo.from (that:Integer) = Foo.F 100*that
        Bar.from (that:Integer) = Bar.B 1000*that

        main =
           a = 42 : (Foo&Bar)
           Runtime.assert <| Meta.is_a a Foo
           Runtime.assert <| Meta.is_a a Bar
           b = Bar.from a
           f = Foo.from a
           [a, b, f]

        """;
    var arr = ctx.evalModule(src);
    assertTrue("It is array", arr.hasArrayElements());
    assertEquals("Three elements", 3, arr.getArraySize());
    var a = arr.getArrayElement(0);
    var b = arr.getArrayElement(1);
    var f = arr.getArrayElement(2);

    assertTypes("Two types for a", a, true, "Foo", "Bar");
    assertTypes("Both types are visible", a, false, "Foo", "Bar");
    assertTypes("Two types for b", b, true, "Bar", "Foo");
    assertTypes("Only Bar is visible", b, false, "Bar");
    assertTypes("Two types", f, true, "Foo", "Bar");
    assertTypes("Only Foo is visible", f, false, "Foo");
  }

  @Test
  public void testNoConversionWhenMultiValueMatchesTwoOfThee() {
    String src =
        """
        from Standard.Base import Any, Integer, Meta, Runtime

        type Foo
           F n
        type Bar
           B n
        type Car
           C n

        Foo.from (that:Integer) = Foo.F 100*that
        Bar.from (that:Integer) = Bar.B 1000*that
        Car.from (that:Integer) = Car.C 10000*that

        main =
           a = 42 : (Foo&Bar&Car)
           Runtime.assert <| Meta.is_a a Foo
           Runtime.assert <| Meta.is_a a Bar
           Runtime.assert <| Meta.is_a a Car
           cb = a : (Car&Bar)
           fc = a : (Foo&Car)
           [a, cb, fc]

        """;
    var arr = ctx.evalModule(src);
    assertTrue("It is array", arr.hasArrayElements());
    assertEquals("Three elements", 3, arr.getArraySize());
    var a = arr.getArrayElement(0);
    var cb = arr.getArrayElement(1);
    var fc = arr.getArrayElement(2);

    assertTypes("Three types for a", a, true, "Foo", "Bar", "Car");
    assertTypes("All types are visible", a, false, "Foo", "Bar", "Car");
    assertTypes("Three types for cb", cb, true, "Car", "Bar", "Foo");
    assertTypes("Only Car and Bar are visible", cb, false, "Car", "Bar");
    assertTypes("Three types for fc", fc, true, "Foo", "Car", "Bar");
    assertTypes("Only Foo and Car are visible", fc, false, "Foo", "Car");
  }

  @Test
  public void testApplyConversionToSelfArgument() {
    var src =
        """
        type A
            Cons

            method self =
                B.method self=self

        type B
            Cons

            method self =
                self

        B.from (that:A) = B.Cons

        main =
            a = A.Cons
            a.method
        """;
    var aMethod = ctx.evalModule(src);
    assertTypes("Should be B", aMethod, false, "B");
  }

  private static void assertTypes(
      String msg, Value value, boolean includeExtraTypes, String... expectedTypes) {
    var v = ctx.unwrapValue(value);
    var vt = TypeOfNode.getUncached().findAllTypesOrNull(v, includeExtraTypes);
    var vtn = Stream.of(vt).map((t) -> t.toString()).toArray();
    Assert.assertArrayEquals(msg, expectedTypes, vtn);
  }
}
