package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.After;
import org.junit.ClassRule;
import org.junit.Test;

public class ConversionMethodTests {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @After
  public void resetOutput() {
    ctxRule.resetOut();
  }

  private String getStdOut() {
    return ctxRule.getOut();
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
    Value res = ctxRule.evalModule(src);
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
    Value res = ctxRule.evalModule(src);
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
    Value res = ctxRule.evalModule(src);
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
    Value res = ctxRule.evalModule(src);
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
      Value res = ctxRule.evalModule(src);
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
}
