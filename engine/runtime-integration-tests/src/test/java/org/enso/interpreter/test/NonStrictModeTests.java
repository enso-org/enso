package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Map;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class NonStrictModeTests extends TestBase {
  private static Context nonStrictCtx;
  private static MockLogHandler logHandler;

  @BeforeClass
  public static void initCtx() {
    logHandler = new MockLogHandler();
    nonStrictCtx = createNonStrictContext();
  }

  protected static Context createNonStrictContext() {
    var context =
        defaultContextBuilder()
            .logHandler(logHandler)
            .option(RuntimeOptions.STRICT_ERRORS, "false")
            .build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    return context;
  }

  @AfterClass
  public static void disposeCtx() {
    nonStrictCtx.close();
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
    Value res = evalModule(nonStrictCtx, src);
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

    Value res = evalModule(nonStrictCtx, src);
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
    Value res = evalModule(nonStrictCtx, src);
    assertEquals(4, res.asInt());

    String line1 =
        "Unnamed:1:1: error: Package containing the module That.Does.Not.Exist could not be loaded:"
            + " The package could not be resolved: The library `That.Does` is not defined within"
            + " the edition.";
    String line2 = "    1 | import That.Does.Not.Exist";
    String line3 = "      | ^~~~~~~~~~~~~~~~~~~~~~~~~~";
    logHandler.assertMessage(
        "enso.org.enso.compiler.Compiler", line1 + "\n" + line2 + "\n" + line3);
  }
}
