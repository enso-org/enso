package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ConversionMethodTests extends TestBase {
  private static Context ctx;

  @BeforeClass
  public static void initCtx() {
    ctx = createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  @Test
  public void testSimpleConversion() {
    String src = """
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
    String src = """
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
    String src = """
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
    String src = """
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
}
