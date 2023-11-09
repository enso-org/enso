package org.enso.interpreter.test;

import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class NonStrictConversionMethodTests extends TestBase {
  private static Context nonStrictCtx;

  @BeforeClass
  public static void initCtx() {
    nonStrictCtx = createNonStrictContext();
  }

  protected static Context createNonStrictContext() {
    var context =
        defaultContextBuilder().option(RuntimeOptions.STRICT_ERRORS, "false").build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    return context;
  }

  @AfterClass
  public static void disposeCtx() {
    nonStrictCtx.close();
  }

  @Test
  public void testAmbiguousConversion() {
    String src = """      
       type Foo
          Mk_Foo data
       type Bar
          Mk_Bar x
       
       Foo.from (that:Bar) = Foo.Mk_Foo that.x+100
       Foo.from (that:Bar) = Foo.Mk_Foo that.x+1000
       
       main = 42
       """;
    Value res = evalModule(nonStrictCtx, src);
    assertEquals(42, res.asInt());
  }

  @Test
  public void testAmbiguousConversionUsage() {
    // In non-strict mode, the conversion declarations will have errors attached to the IR, but the overall operation
    // will simply not see the second conversion and succeed with the first one.
    String src = """      
       type Foo
          Mk_Foo data
       type Bar
          Mk_Bar x
       
       Foo.from (that:Bar) = Foo.Mk_Foo that.x+100
       Foo.from (that:Bar) = Foo.Mk_Foo that.x+1000
       
       main = (Foo.from (Bar.Mk_Bar 42)) . data
       """;

    Value res = evalModule(nonStrictCtx, src);
    assertEquals(142, res.asInt());
  }

}
