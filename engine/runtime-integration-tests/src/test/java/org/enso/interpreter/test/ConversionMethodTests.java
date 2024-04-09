package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class ConversionMethodTests extends TestBase {
  private static Context ctx;

  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void initCtx() {
    ctx = createDefaultContext(out);
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Before
  public void resetOutput() {
    out.reset();
  }

  private String getStdOut() {
    return out.toString(StandardCharsets.UTF_8);
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
    Value res = evalModule(ctx, src);
    assertEquals(30, res.asInt());
  }

  @Test
  public void testDispatchOnHostMap() {
    String src =
        """
       polyglot java import java.util.Map as Java_Map
       import Standard.Base.Data.Map.Map

       type Foo
          Mk_Foo data

       Foo.from (that:Map) = Foo.Mk_Foo that

       main =
           jmap = Java_Map.of "A" 1 "B" 2 "C" 3
           Foo.from jmap . data . size
       """;
    Value res = evalModule(ctx, src);
    assertEquals(3, res.asInt());
  }

  @Test
  public void testDispatchOnJSMap() {
    String src =
        """
       import Standard.Base.Data.Map.Map

       foreign js js_map = '''
           let m = new Map()
           m.set("A", 1)
           m.set("B", 2)
           return m

       type Foo
          Mk_Foo data

       Foo.from (that:Map) = Foo.Mk_Foo that

       main =
           Foo.from js_map . data . size
       """;
    Value res = evalModule(ctx, src);
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
    Value res = evalModule(ctx, src);
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
      Value res = evalModule(ctx, src);
      fail("Expected an exception, but got " + res);
    } catch (Exception e) {
      assertEquals("Compilation aborted due to errors.", e.getMessage());
      MatcherAssert.assertThat(
          getStdOut(),
          Matchers.containsString(
              "Unnamed:7:1: error: Ambiguous conversion: Foo.from Bar is defined multiple times in"
                  + " this module."));
    }
  }
}
