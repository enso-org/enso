package org.enso.compiler.test;

import static org.enso.compiler.test.ExecCompilerTest.ctxRule;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.hamcrest.core.AllOf;
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
      var module =
          ctxRule.eval(
              "enso",
              """
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
    var code =
        """
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

  @Test
  public void castToUnresolvedType() throws Exception {
    var code =
        """
        from Standard.Base import all
        fn f = (f : Unknown).to_text
        """;
    try {
      var module = ctxRule.eval(LanguageInfo.ID, code);
      var fn = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");
      var r = fn.execute(0);
      fail("We don't expect any result, but exception: " + r);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(containsString("Unknown"), containsString("could not be found")));
    }
  }

  @Test
  public void blockAppliedToUnknownSymbol() throws Exception {
    var code =
        """
        from Standard.Base import all
        fn =
            f
                10
        """;
    try {
      var module = ctxRule.eval(LanguageInfo.ID, code);
      var fn = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fn");
      var r = fn.execute();
      fail("We don't expect any result, but exception: " + r);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(containsString("The name `f`"), containsString("could not be found")));
    }
  }

  @Test
  public void suspendedDefaultedUnionArgument() throws Exception {
    var code =
        """
        from Standard.Base import all
        def a:Integer ~b:Text|Nothing=Nothing -> Text|Nothing =
            if a < 0 then "Minus" else
                b
        call_def_with_thunk a:Integer =
            def a 6*7
        """;
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var def = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "def");
    var hi = def.execute(1, "Hi");
    assertEquals("Hi", hi.asString());
    try {
      var noResult = def.execute(-2, 20);
      fail("Invoking def with second argument being Integer yields an exception: " + noResult);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(
              containsString("expected `b` to be Text"), containsString("but got Integer")));
    }
    assertTrue("Default value is Nothing. Returns Nothing.", def.execute(3).isNull());
    assertTrue("Passing null is OK.", def.execute(4, null).isNull());
    var thunkArg = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "call_def_with_thunk");
    var m = thunkArg.execute(-1);
    assertEquals(
        "Invoking def with second argument being a Thunk passes the type check",
        "Minus",
        m.asString());
    try {
      var fail = thunkArg.execute(1);
      fail(
          "Non-negative first argument requires evaluation of the second and that fails on type"
              + " check.");
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(
              containsString("expected `b` to be Text"), containsString("but got Integer")));
    }
  }

  @Test
  public void onlyElse() throws Exception {
    var code =
        """
        from Standard.Base import all
        def a:Integer =
            else a
        """;
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var def = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "def");
    try {
      var noResult = def.execute(20);
      fail("Yields an error: " + noResult);
    } catch (PolyglotException ex) {
      assertThat(ex.getMessage(), containsString("no branch matches"));
    }
  }

  @Test
  public void missingElseBranch() throws Exception {
    var code =
        """
        from Standard.Base import all
        def a:Boolean ~b c =
            if a then
                b
            else
            node = c
            node
        """;
    try {
      var module = ctxRule.eval(LanguageInfo.ID, code);
      fail("Compilation produces an error, not a module: " + module);
    } catch (PolyglotException ex) {
      assertThat(
          "In strict mode the error happens when compiling the module. No execution is needed.",
          ex.getMessage(),
          containsString("error: Missing else branch."));
    }
  }
}
