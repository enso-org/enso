package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}

class MethodsTest extends InterpreterTest {
  "Methods" should "be defined in the global scope and dispatched to" in {
    val code =
      """
        |type Foo
        |Foo.bar = number -> number + 1
        |bar Foo 10
        |""".stripMargin
    eval(code) shouldEqual 11
  }

  "Methods" should "be callable with dot operator" in {
    val code =
      """
        |type Foo
        |Foo.bar = number -> number + 1
        |Foo.bar 10
        |""".stripMargin
    eval(code) shouldEqual 11
  }

  "Methods" should "be chainable with dot operator" in {
    val code =
      """
        |type Foo
        |type Bar
        |type Baz
        |
        |Foo.bar = Bar
        |Bar.baz = x -> Baz
        |Baz.spam = y -> y + 25
        |
        |Foo.bar.baz 54 . spam 2
        |""".stripMargin
    eval(code) shouldEqual 27
  }

  "Dot operator" should "behave like parenthesised when non-spaced" in {
    val code =
      """
        |type Foo
        |type Bar
        |
        |Foo.bar = a b -> a + b
        |Bar.constant = 10
        |
        |Foo.bar Bar.constant Bar.constant
        |
        |""".stripMargin
    eval(code) shouldEqual 20
  }

  "Methods" should "be able to be defined without arguments" in {
    val code =
      """
        |type Foo
        |Foo.bar = 1
        |bar Foo + 5
        |""".stripMargin
    eval(code) shouldEqual 6
  }

  "Methods" should "be definable as blocks without arguments" in {
    val code =
      """
        |Any.method =
        |    x = this * this
        |    y = x * 2
        |    y + 1
        |    
        |3.method
        |""".stripMargin
    eval(code) shouldEqual 19
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

    evalOld(code) shouldEqual 3
  }

  "Method call target" should "be passable by-name" in {
    val code =
      """
        |Unit.testMethod = x y z -> x + y + z
        |testMethod x=1 y=2 this=Unit z=3
        |""".stripMargin

    eval(code) shouldEqual 6
  }

  "Calling a non-existent method" should "throw an exception" in {
    val code =
      """
        |foo 7
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
    evalOld(code)
    consumeOut shouldEqual List("1", "2", "3", "0", "0", "0")
  }

  "Test" should "test test" in {
    pending
//    val code =
//      """
//        |Nil.sum = 0
//        |Cons.sum = case this of
//        |  Cons h t -> h + sum t
//        |
//        |myList = Cons (Cons (Cons 3 Nil) 2) 1
//        |
//        |""".stripMargin
    val code =
      """
        |Cons.sum = case a of
        |  b
        |""".stripMargin

    eval(code) shouldEqual 6
  }
}
