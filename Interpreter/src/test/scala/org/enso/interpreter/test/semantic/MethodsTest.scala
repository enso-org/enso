package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, LanguageTest}

class MethodsTest extends LanguageTest {
  "Methods" should "be defined in the global scope and dispatched to" in {
    val code =
      """
        |type Foo;
        |Foo.bar = { |number| number + 1 }
        |@bar [@Foo, 10]
        |""".stripMargin
    eval(code) shouldEqual 11
  }

  "Methods" should "be dispatched to the proper constructor" in {
    val code =
      """
        |Nil.sum = { |acc| acc }
        |Cons.sum = { |acc| match this <
        |  Cons ~ { |h, t| @sum [t, h + acc] };
        |>}
        |
        |@sum [@Cons [1, @Cons [2, @Nil]], 0]
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Method call target" should "be passable by-name" in {
    val code =
      """
        |Unit.testMethod = { |x, y, z| (x + y) + z }
        |@testMethod [x = 1, y = 2, this = @Unit, z = 3]
        |""".stripMargin

    eval(code) shouldEqual 6
  }

  "Calling a non-existent method" should "throw an exception" in {
    val code =
      """
        |@foo [7]
        |""".stripMargin
    the[InterpreterException] thrownBy eval(code) should have message "Object Number does not define method foo."
  }

  "Methods defined on Any type" should "be callable for any type" in {
    val code =
      """
        |type Foo;
        |type Bar;
        |type Baz;
        |
        |Any.method = { match this <
        |  Foo ~ { 1 };
        |  Bar ~ { 2 };
        |  Baz ~ { 3 };
        |  { 0 };
        |>}
        |
        |@{
        |  @println [@IO, @method [@Foo]];
        |  @println [@IO, @method [@Bar]];
        |  @println [@IO, @method [@Baz]];
        |  @println [@IO, @method [@Unit]];
        |  @println [@IO, @method [123]];
        |  @println [@IO, @method [{|x| x }]];
        |  0
        |}
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("1", "2", "3", "0", "0", "0")
  }
}
