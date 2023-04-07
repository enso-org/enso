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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

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

  @Test
  public void testPrintText() throws Exception {
    final String code = """
    import Standard.Base.IO

    test _ =
        IO.println "Foobar"
    """;

    var test = evalCode(code, "test");
    test.execute(0);
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("Foobar\n", log);
  }

  @Test
  public void testPrintPrimitive() throws Exception {
    final String code = """
    import Standard.Base.IO

    test =
        IO.println 42
    """;

    var test = evalCode(code, "test");
    test.execute();
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("42\n", log);
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

    var test = evalCode(code, "test");
    test.execute();
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("MyObj{42}\n", log);
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

    var test = evalCode(code, "test");
    test.execute();
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("(Error: My_Error.Error 1)\n", log);
  }

  @Test
  public void testPrintToTextHasWarnings() throws Exception {
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

    var test = evalCode(code, "test");
    test.execute();
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("MyObj{42}\n", log);
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

    var test = evalCode(code, "test");
    test.execute();
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("100\n", log);
  }

  // This test documents the current, but undesirable behaviour. It may be removed if it is fixed.
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

    var test = evalCode(code, "test");
    test.execute();
    String log = out.toString(StandardCharsets.UTF_8);
    assertEquals("???\n", log);
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
}
