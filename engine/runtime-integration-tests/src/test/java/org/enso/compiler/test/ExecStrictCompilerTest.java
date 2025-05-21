package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.After;
import org.junit.ClassRule;
import org.junit.Test;

public class ExecStrictCompilerTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(ctxBldr -> ctxBldr.option(RuntimeOptions.STRICT_ERRORS, "true"))
          .build();

  @After
  public void cleanMessages() {
    ctxRule.resetOut();
  }

  @Test
  public void redefinedArgument() {
    try {
      var module = ctxRule.eval("enso", """
      type My_Type
          Value a b c a
      """);
      fail("Expecting no returned value: " + module);
    } catch (PolyglotException ex) {
      assertTrue("Syntax error", ex.isSyntaxError());
      assertTrue("Guest exception", ex.isGuestException());
      assertThat(
          ex.getMessage(),
          containsString(
              "Unnamed:2:17: error: Redefining arguments is not supported: a is defined multiple"
                  + " times."));

      var errors = ctxRule.getOut();
      assertNotEquals(
          "Errors reported in " + errors,
          -1,
          errors.indexOf("Redefining arguments is not supported"));
      assertNotEquals(
          "Identifier recognized in " + errors, -1, errors.indexOf("a is defined multiple times"));
    }
  }

  @Test
  public void testUnknownConstructorLocation() throws Exception {
    var code =
        Source.newBuilder(
                "enso",
                """
                foo x = case x of
                    Index_Sub_Range.Sample _ _ -> 1
                    _ -> 2
                """,
                "wrong_cons.enso")
            .build();
    try {
      var module = ctxRule.eval(code);
      fail("Expecting no returned value: " + module);
    } catch (PolyglotException ex) {
      assertTrue("Syntax error", ex.isSyntaxError());
      assertTrue("Guest exception", ex.isGuestException());
      assertThat(
          ex.getMessage(), containsString("The name `Index_Sub_Range.Sample` could not be found."));

      var errors = ctxRule.getOut();
      assertNotEquals(
          "Errors reported in " + errors,
          -1,
          errors.indexOf("The name `Index_Sub_Range.Sample` could not be found"));
      assertNotEquals("Location defined " + errors, -1, errors.indexOf("wrong_cons:2:5"));
    }
  }

  @Test
  public void testUnknownTypeExtensionMethod() throws Exception {
    var code = """
    Unknown_Type.foo = 42

    main = 42
    """;
    var src = Source.newBuilder("enso", code, "extension.enso").build();
    try {
      var module = ctxRule.eval(src);
      fail("Unexpected result: " + module);
    } catch (PolyglotException ex) {
      var firstLine = ex.getMessage().split("\n")[0];
      assertEquals("extension:1:1: error: The name `Unknown_Type` could not be found.", firstLine);
    }
  }

  @Test
  public void fqnAreAllowedInTypeSignatures() {
    var code =
        """
        from Standard.Base import all

        foo : Standard.Base.Data.Numbers.Integer
        foo = 1

        bar (x : Standard.Base.Data.Numbers.Integer) = 10+x

        main =
            bar foo
        """;
    var res = ctxRule.evalModule(code);
    assertTrue("Compiles and returns result", res.isNumber());
    assertEquals("Returns correct result", 11, res.asInt());
  }

  /*
   * https://github.com/enso-org/enso/issues/12376
   * naming_helper was removed in a refactor. Replaced with a similar situation
   * where `parse_simple_date_pattern` is both a method on a type and standalone
   * method with same name in the same module.
   */
  @Test
  public void noDuplicateImportWarning() {
    var code =
        """
        from Standard.Base.Internal.Time.Format.Parser import parse_simple_date_pattern

        main =
            parse_simple_date_pattern
        """;
    var res = ctxRule.evalModule(code);
    assertThat(res, is(notNullValue()));
    var errors = ctxRule.getOut();
    assertThat(
        "There should be no errors or warnings. But there was: " + errors,
        errors.isEmpty(),
        is(true));
  }
}
