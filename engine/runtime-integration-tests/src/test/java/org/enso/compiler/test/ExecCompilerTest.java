package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.MethodNames.Module;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.expression.errors.Conversion.DeclaredAsPrivate$;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.hamcrest.core.AllOf;
import org.junit.After;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;

public class ExecCompilerTest {
  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(ctxBldr -> ctxBldr.option(RuntimeOptions.STRICT_ERRORS, "false"))
          .build();

  @After
  public void cleanup() {
    ctxRule.resetOut();
  }

  @Test
  public void testCaseOfWithNegativeConstant() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            run value =
                case value of
                    -1 -> "minus one"
                    _ -> "none"
            """);
    var run = module.invokeMember("eval_expression", "run");
    var minusOne = run.execute(-1);
    assertEquals("minus one", minusOne.asString());
    var none = run.execute(33);
    assertEquals("none", none.asString());
  }

  @Test
  public void testDesugarOperators() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            main =
              ma ==ums==
            """);
    try {
      var run = module.invokeMember("eval_expression", "main");
      fail("Unexpected result: " + run);
    } catch (PolyglotException ex) {
      assertEquals("Compile error: The name `ma` could not be found.", ex.getMessage());
    }
  }

  @Test
  public void testDesugarOperatorsLeftRight() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            main = (+ (2 *))
            """);
    try {
      var run = module.invokeMember("eval_expression", "main");
      fail("Unexpected result: " + run);
    } catch (PolyglotException ex) {
      assertEquals("Method `+` of type Unnamed could not be found.", ex.getMessage());
    }
  }

  @Test
  public void testDesugarOperatorsRightLeft() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            main = ((* 2) +)
            """);
    try {
      var run = module.invokeMember("eval_expression", "main");
      fail("Unexpected result: " + run);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage(),
          AllOf.allOf(
              containsString("Method `+` of"),
              containsString("Unnamed.main.Unnamed.main"),
              containsString("could not be found.")));
    }
  }

  @Test
  public void testHalfAssignment() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base.Errors.Common import all
            run value =
                x = 4
                y =
                z = 5
            """);
    var run = module.invokeMember("eval_expression", "run");
    try {
      var never = run.execute(-1);
      fail("Unexpected result: " + never);
    } catch (PolyglotException ex) {
      assertEquals("Syntax error: Unexpected expression.", ex.getMessage());
    }
  }

  @Test
  public void redefinedArgument() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            type My_Type
                Value a b c a
            """);
    var run = module.invokeMember("eval_expression", "My_Type.Value");
    var atom = run.newInstance(1, 2, 3, 4);
    assertFalse("In spite of error we get an instance back: " + atom, atom.isException());
    assertTrue("Check a: " + atom.getMemberKeys(), atom.getMemberKeys().contains("a"));
    assertTrue("Check b: " + atom.getMemberKeys(), atom.getMemberKeys().contains("b"));
    assertTrue("Check c: " + atom.getMemberKeys(), atom.getMemberKeys().contains("c"));

    assertEquals("c", 3, atom.getMember("c").asInt());
    assertEquals("b", 2, atom.getMember("b").asInt());
    assertEquals("First value of a is taken", 1, atom.getMember("a").asInt());
  }

  @Test
  public void testSelfAssignment() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base.Errors.Common import all
            run value =
                meta1 = meta1
                meta1
            """);
    var run = module.invokeMember("eval_expression", "run");
    var error = run.execute(-1);
    assertTrue("We get an error value back", error.isException());
    assertTrue("The error value also represents null", error.isNull());
    assertEquals("(Error: 'Uninitialized value')", error.toString());
  }

  @Test
  public void testRecursiveDefinition() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base import all

            run prefix =
                op = if False then 42 else prefix+op
                op
            """);
    var run = module.invokeMember("eval_expression", "run");
    var error = run.execute("Nope: ");
    assertTrue("We get an error value back", error.isException());
    assertTrue("The error value also represents null", error.isNull());
    assertEquals("(Error: 'Uninitialized value')", error.toString());
  }

  @Ignore("Explicitly-default arguments will be implemented in #8480")
  @Test
  public void testDefault() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            f x=1 = x
            value_from_default =
              f default
            """);
    var result = module.invokeMember("eval_expression", "value_from_default");
    assertEquals("Value obtained from default argument", 1, result.asInt());
  }

  @Test
  public void testIdentCalledDefault() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            f x=1 = x
            value_from_binding =
              default = 2
              f default
            """);
    var result = module.invokeMember("eval_expression", "value_from_binding");
    assertEquals("Value obtained from binding", 2, result.asInt());
  }

  @Test
  public void dotUnderscore() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            run op =
              op._
            """);
    var run = module.invokeMember("eval_expression", "run");
    try {
      var error = run.execute("false_hope");
      fail("Should never return, but: " + error);
    } catch (PolyglotException e) {
      assertTrue("It is exception", e.getGuestObject().isException());
      assertEquals("Panic", e.getGuestObject().getMetaObject().getMetaSimpleName());
      if (!e.getMessage().contains("Invalid use of")) {
        fail("Expecting Invalid use of syntactic operator, but was: " + e.getMessage());
      }
    }
  }

  @Test
  public void chainedSyntax() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base import all

            nums n = [1, 2, 3, 4, 5]
                . map (x-> x*2)
                . filter (x-> x % 3 == 0)
                . take n
            """);
    var run = module.invokeMember("eval_expression", "nums");
    var six = run.execute(1);
    assertTrue(six.hasArrayElements());
    assertEquals(1, six.getArraySize());
    assertEquals(6, six.getArrayElement(0).asInt());
  }

  @Test
  public void chainedSyntaxOperator() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            nums n = n
                * 2
                % 3
            """);
    var run = module.invokeMember("eval_expression", "nums");
    var result = run.execute(5);
    assertEquals("10 % 3 is one", 1, result.asInt());
  }

  @Test
  public void directArgumentToASymbol() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            nums n =
                f x = x * 2
                f n
            """);
    var run = module.invokeMember("eval_expression", "nums");
    var result = run.execute(5);
    assertEquals("Twice five", 10, result.asInt());
  }

  @Test
  public void blockArgumentToASymbol() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            nums n =
                f x = x * 2
                f
                    n
            """);
    var run = module.invokeMember("eval_expression", "nums");
    var result = run.execute(5);
    assertEquals("Twice five", 10, result.asInt());
  }

  @Test
  public void inlineReturnSignature() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            import Standard.Base.Data.Numbers.Integer
            foo (x : Integer) (y : Integer) -> Integer = 10*x + y
            """);
    var foo = module.invokeMember("eval_expression", "foo");
    assertTrue("foo a function", foo.canExecute());
    assertEquals(45, foo.execute(4, 5).asInt());
  }

  @Test
  public void inlineReturnSignatureOnMemberMethod() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            import Standard.Base.Data.Numbers
            import Standard.Base.Data.Numbers.Integer
            type My_Type
                Value x

                member_foo self (y : Integer) z -> Integer = 100*z + 10*y + self.x
            """);
    var instance = module.invokeMember("eval_expression", "My_Type.Value 1");
    var result = instance.invokeMember("member_foo", 2, 3);
    assertEquals(321, result.asInt());
  }

  @Test
  public void inlineWithBlanks() {
    var code =
        """
        remap rows:Map -> Map =
            rows.map (_.set_u 5)

        type Map
            M v u=3

            map self fn = Map.M (fn self)
            set_u self i = Map.M self.v i

        run n = remap (Map.M n)
        """;
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var run = module.invokeMember("eval_expression", "run");
    assertTrue("run is a function", run.canExecute());
    assertEquals("(M (M 10 5) 3)", run.execute(10).toString());
  }

  @Test
  public void inlineReturnSignatureOnLocalFunction() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            import Standard.Base.Data.Numbers.Integer
            foo x y =
                inner_foo (z : Integer) -> Integer = 100*z + 10*y + x
                a = 3
                r = inner_foo a
                r + 50000
            """);
    var instance = module.invokeMember("eval_expression", "foo");
    var result = instance.execute(1, 2);
    assertEquals(50321, result.asInt());
  }

  @Test
  public void inlineReturnSignatureWithoutArguments() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            import Standard.Base.Data.Numbers.Integer
            the_number -> Integer = 23
            """);
    var result = module.invokeMember("eval_expression", "the_number");
    assertEquals("Function-return syntax can be used with 0 arguments", 23, result.asInt());
  }

  /**
   * This test demonstrates a slightly un-intuitive, but apparently needed by our rules, behaviour
   * of `->` with ascriptions: 1. for `foo a:Integer -> Integer` it is interpreted as foo
   * (a:Integer) -> Integer - i.e. a function taking an Integer and returning an Integer. 2. for
   * `foo a : Integer -> Integer`, this results in a compile error currently.
   */
  @Test
  public void weirdReturnTypeSignature1() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                from Standard.Base import Integer
                foo a:Integer -> Integer = a+10
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    var module = ctxRule.eval(src);
    var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
    assertEquals(11, foo.execute(1).asInt());
  }

  @Test
  public void weirdReturnTypeSignature2() throws Exception {
    final URI uri = new URI("memory://rts.enso");
    final Source src =
        Source.newBuilder(
                LanguageInfo.ID,
                """
                from Standard.Base import Integer
                foo a : Integer -> Integer = a+10
                """,
                uri.getAuthority())
            .uri(uri)
            .buildLiteral();

    try {
      var module = ctxRule.eval(src);
      var foo = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "foo");
      fail("Compiler error was expected, but foo evaluated successfully as: " + foo);
    } catch (PolyglotException ex) {
      assertEquals("Compile error: The name `foo` could not be found.", ex.getMessage());
    }
  }

  @Test
  public void testInvalidEnsoProjectRef() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base.Errors.Common import all
            from Standard.Base.Meta.Enso_Project import enso_project
            run dummy =
                _ = dummy
                (enso_project.data / "foo").to_display_text
            """);
    var run = module.invokeMember("eval_expression", "run");
    var err = run.execute(0);
    assertEquals("Error: Module is not a part of a package.", err.asString());
  }

  @Test
  public void testDoubledRandom() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base import all
            polyglot java import java.util.Random

            run seed =
                Random.new_generator seed
            """);
    var run = module.invokeMember("eval_expression", "run");
    try {
      var err = run.execute(1L);
      assertTrue("Returned value represents exception: " + err, err.isException());
      throw err.throwException();
    } catch (PolyglotException ex) {
      assertEquals("Compile error: No polyglot symbol for Random.", ex.getMessage());
    }
  }

  @Test
  public void testUnknownStaticField() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base import all
            polyglot java import java.util.Random as R

            run seed = case seed of
                R.NO_FIELD -> 0
                _ -> -1
            """);
    var run = module.invokeMember("eval_expression", "run");
    try {
      var err = run.execute(1L);
      fail("Not expecting any result: " + err);
    } catch (PolyglotException ex) {
      assertEquals("Compile error: NO_FIELD is not visible in this scope.", ex.getMessage());
    }
  }

  @Test
  public void testFnAsADefaultValue() {
    var code =
        """
        type N
        type T
            V (r:(T -> N | T)=(_->N))

            v self = self.r self

        run type = case type of
          0 -> T.V
          1 -> T.V (_->N)
        """;
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var run = module.invokeMember("eval_expression", "run");
    var real = run.execute(1L);
    var realN = real.invokeMember("v");
    var defaulted = run.execute(0L);
    var defaultedN = defaulted.invokeMember("v");
    assertEquals("Should be the same", realN, defaultedN);
  }

  @Test
  public void testTemporaryFileSpecProblem() {
    var code =
        """
        from Standard.Base.Errors.Common import all

        run t = F.app f->
          f.read t

        type F
          read self r = r
          app fn = fn F
        """;
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var run = module.invokeMember("eval_expression", "run");
    var real = run.execute(1L);
    assertEquals("Should be the same", 1, real.asInt());
  }

  @Test
  public void testPropertlyIdentifyNameOfJavaClassInError() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            from Standard.Base.Errors.Common import all
            polyglot java import java.lang.Runnable

            run value =
                Runnable.invoke value
            """);
    var run = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "run");
    try {
      var never = run.execute(-1);
      fail("Unexpected result: " + never);
    } catch (PolyglotException ex) {
      assertEquals(
          "Method `invoke` of type java.lang.Runnable could not be found.", ex.getMessage());
    }
  }

  // Conversion methods cannot be specified as private
  @Test
  public void illegalPrivateConversion() {
    var module =
        ctxRule.eval(
            LanguageInfo.ID,
            """
            type My_Type
            type Other_Type
            private My_Type.from (other:Other_Type) =
                42
            run value =
                42
            """);
    var expectedErrMsg = DeclaredAsPrivate$.MODULE$.explain();
    var runMethod = module.invokeMember(Module.EVAL_EXPRESSION, "run");
    runMethod.execute(0);
    assertThat(ctxRule.getOut(), containsString(expectedErrMsg));
  }

  @Test
  public void resultOfConversionIsTypeChecked() {
    var code =
        """
        type First_Type
        type Other_Type

        First_Type.from (that:Other_Type) = 42
        run value -> First_Type = Other_Type
        """;
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var runMethod = module.invokeMember(Module.EVAL_EXPRESSION, "run");
    try {
      var r = runMethod.execute(0);
      fail("We don't expect any result, but exception: " + r);
    } catch (PolyglotException ex) {
      assertThat(
          ex.getMessage().toLowerCase(),
          AllOf.allOf(containsString("type"), containsString("error")));
      var typeError = ex.getGuestObject();
      assertEquals("Expected type", "First_Type", typeError.getMember("expected").asString());
      assertEquals("Got wrong value", 42, typeError.getMember("actual").asInt());
    }
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
          AllOf.allOf(
              containsString("expected unresolved symbol Unknown"),
              containsString("to be resolved to a type")));
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
    var module = ctxRule.eval(LanguageInfo.ID, code);
    var def = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "def");
    try {
      var noResult = def.execute(true, 6, 7);
      fail("Yields an error: " + noResult);
    } catch (PolyglotException ex) {
      assertThat(
          "In non-strict mode the error happens when executing the def function.",
          ex.getMessage(),
          containsString("error: Missing else branch."));
    }
  }

  @Test
  public void ifThenElseInABlockApplication() throws Exception {
    var code =
        """
        act n a b =
          fn = (* 2)
          fn
              if n<5 then
                  a+b
              else
                  a*b
        """;

    var module = ctxRule.eval(LanguageInfo.ID, code);
    var act = module.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "act");

    assertEquals(26, act.execute(3, 6, 7).asInt());
    assertEquals(84, act.execute(7, 6, 7).asInt());
  }
}
