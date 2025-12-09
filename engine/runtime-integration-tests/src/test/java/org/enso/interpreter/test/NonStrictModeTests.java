package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;

import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class NonStrictModeTests {
  private static final MockLogHandler logHandler = new MockLogHandler();

  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(
              b -> b.logHandler(logHandler).option(RuntimeOptions.STRICT_ERRORS, "false"))
          .build();

  @AfterClass
  public static void dispose() {
    logHandler.close();
  }

  @Before
  public void resetOutput() {
    logHandler.reset();
  }

  @Test
  public void testAmbiguousConversion() {
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
    Value res = ctxRule.evalModule(src);
    assertEquals(42, res.asInt());

    // Even if the conversion is unused and non-strict mode, we still get a diagnostic report:
    logHandler.assertMessage(
        "enso.org.enso.compiler.Compiler",
        "Unnamed:7:1: error: Ambiguous conversion: Foo.from Bar is defined multiple times in this"
            + " module.");
  }

  @Test
  public void testAmbiguousConversionUsage() {
    // In non-strict mode, the conversion declarations will have errors attached to the IR, but the
    // overall operation
    // will simply not see the second conversion and succeed with the first one.
    String src =
        """
        import Standard.Base.Data.Numbers

        type Foo
           Mk_Foo data
        type Bar
           Mk_Bar x

        Foo.from (that:Bar) = Foo.Mk_Foo that.x+100
        Foo.from (that:Bar) = Foo.Mk_Foo that.x+1000

        main = (Foo.from (Bar.Mk_Bar 42)) . data
        """;

    Value res = ctxRule.evalModule(src);
    assertEquals(142, res.asInt());

    logHandler.assertMessage(
        "enso.org.enso.compiler.Compiler",
        "Unnamed:9:1: error: Ambiguous conversion: Foo.from Bar is defined multiple times in this"
            + " module.");
  }

  @Test
  public void testBadImport() {
    String src =
        """
        import That.Does.Not.Exist
        import Standard.Base.Data.Numbers

        main = 2+2
        """;
    Value res = ctxRule.evalModule(src);
    assertEquals(4, res.asInt());

    String line1 =
        "Unnamed:1:1: error: Package containing the module That.Does.Not.Exist could not be loaded:"
            + " The package could not be resolved: The library `That.Does` is not defined within"
            + " the edition";
    String line2 = "    1 | import That.Does.Not.Exist";
    String line3 = "      | ^~~~~~~~~~~~~~~~~~~~~~~~~~";
    var logMessages = logHandler.getRawLogMessages("enso.org.enso.compiler.Compiler");
    assertEquals(1, logMessages.size());
    assertThat(
        logMessages.get(0),
        allOf(containsString(line1), containsString(line2), containsString(line3)));
  }
}
