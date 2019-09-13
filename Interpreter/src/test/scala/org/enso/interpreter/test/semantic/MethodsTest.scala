package org.enso.interpreter.test.semantic

import org.graalvm.polyglot.PolyglotException

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
    the[PolyglotException] thrownBy eval(code) should have message "Object 7 does not define method foo."
  }
}
