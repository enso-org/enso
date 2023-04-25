package org.enso.interpreter.test;

import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Map;

import static org.junit.Assert.*;

public class PrintTest {
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();
  private Context ctx;

  @Before
  public void prepareCtx() {
    this.ctx = Context.newBuilder()
        .allowExperimentalOptions(true)
        .allowIO(true)
        .allowAllAccess(true)
        .logHandler(new ByteArrayOutputStream())
        .out(out)
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    out.reset();
  }

  private void checkPrint(String code, String expected) throws Exception {
    Value result = evalCode(code, "test");
    assertTrue("should return Nothing", result.isNull());
    String log = out.toString(StandardCharsets.UTF_8).trim();
    assertEquals(expected, log);
  }

  private Value evalCode(final String code, final String methodName) throws URISyntaxException {
    final var testName = "test.enso";
    final URI testUri = new URI("memory://" + testName);
    final Source src = Source.newBuilder("enso", code, testName)
        .uri(testUri)
        .buildLiteral();
    var module = ctx.eval(src);
    return module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, methodName);
  }

  @Test
  public void testPrintText() throws Exception {
    final String code = """
    import Standard.Base.IO

    test =
        IO.println "Foobar"
    """;

    checkPrint(code, "Foobar");
  }

  @Test
  public void testPrintPrimitive() throws Exception {
    final String code = """
    import Standard.Base.IO

    test =
        IO.println 42
    """;

    checkPrint(code, "42");
  }

  @Test
  public void testPrintToText() throws Exception {
    final String code = """
    import Standard.Base.IO

    type My_Object
        Value x
        
        to_text self = "MyObj{" + self.x.to_text + "}"

    test =
        IO.println (My_Object.Value 42)
    """;

    checkPrint(code, "MyObj{42}");
  }

  @Test
  public void testPrintError() throws Exception {
    final String code = """
    import Standard.Base.IO
    import Standard.Base.Error.Error

    type My_Error
        Error x

    test =
        a = Error.throw (My_Error.Error 1)
        IO.println a
    """;

    checkPrint(code, "(Error: (My_Error.Error 1))");
  }

  @Test
  public void testPrintToTextHasWarnings() throws Exception {
    final String code = """
    import Standard.Base.IO
    from Standard.Base.Warning import Warning

    test =
        a = Warning.attach "Warning" "FOOBAR"
        IO.println a
    """;

    checkPrint(code, "FOOBAR");
  }

  @Test
  public void testPrintToTextHasWarnings2() throws Exception {
    final String code = """
    import Standard.Base.IO
    from Standard.Base.Warning import Warning

    test =
        a = Warning.attach "Warning" 42
        IO.println a
    """;

    checkPrint(code, "42");
  }

  @Test
  public void testPrintToTextHasWarnings3() throws Exception {
    final String code = """
    import Standard.Base.IO
    from Standard.Base.Warning import Warning

    type My_Object
        Value x
        
        to_text self = "MyObj{" + self.x.to_text + "}"

    test =
        a = Warning.attach "Warning" (My_Object.Value 42)
        IO.println a
    """;

    checkPrint(code, "MyObj{42}");
  }

  @Test
  public void testPrintToTextHasWarnings4() throws Exception {
    final String code = """
    import Standard.Base.IO
    from Standard.Base.Warning import Warning

    type My_Object
        Value x
        
        to_text self =
            res = "MyObj{" + self.x.to_text + "}"
            Warning.attach "Warning2" res

    test =
        a = Warning.attach "Warning" (My_Object.Value 42)
        IO.println a
    """;

    checkPrint(code, "MyObj{42}");
  }

  @Test
  public void testPrintToTextTypeError() throws Exception {
    final String code = """
    import Standard.Base.IO

    type My_Object
        Value x
        
        to_text self = 100

    test =
        a = My_Object.Value 42
        IO.println a
    """;

    checkPrint(code, "100");
  }

  @Test
  public void testPrintToTextTypeErrorAndWarnings() throws Exception {
    final String code = """
    import Standard.Base.IO
    from Standard.Base.Warning import Warning

    type My_Object
        Value x
        
        to_text self =
            res = 100
            Warning.attach "Warning2" res

    test =
        a = Warning.attach "Warning2" (My_Object.Value 42)
        IO.println a
    """;

    checkPrint(code, "100");
  }

  // This test documents the current, but undesirable behaviour. It should be updated when it is fixed.
  @Test
  public void testPrintToTextStaticMethod() throws Exception {
    final String code = """
    import Standard.Base.IO

    type My_Object
        Value x
        
        to_text self = "MyObj{" + self.x.to_text + "}"

    test =
        a = My_Object
        IO.println a
    """;

    checkPrint(code, "My_Object.type.to_text[test:6-16]");
    // We would want the following result:
    //checkPrint(code, "My_Object");
  }
}
