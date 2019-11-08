package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}
import org.graalvm.polyglot.PolyglotException

class NamedArgumentsTest extends InterpreterTest {
  "Functions" should "take arguments by name and use them in their bodies" in {
    val code =
      """
        |Unit.a = 10
        |Unit.addTen = { |b| (@a [@Unit]) + b }
        |
        |@addTen [@Unit, b = 10]
      """.stripMargin

    eval(code) shouldEqual 20
  }

  "Functions" should "be able to have named arguments given out of order" in {
    val code =
      """
        |Unit.subtract = { |a, b| a - b }
        |
        |@subtract [@Unit, b = 10, a = 5]
    """.stripMargin

    eval(code) shouldEqual -5
  }

  "Functions" should "be able to have scope values as named arguments" in {
    val code =
      """
        |@{
        |  a = 10;
        |  addTen = { |num| num + a };
        |  res = @addTen [num = a];
        |  res
        |}
    """.stripMargin

    eval(code) shouldEqual 20
  }

  "Functions" should "be able to be defined with default argument values" in {
    val code =
      """
        |Unit.addNum = { |a, num = 10| a + num }
        |
        |@addNum [@Unit, 5]
    """.stripMargin

    eval(code) shouldEqual 15
  }

  "Default arguments" should "be able to default to complex expressions" in {
    val code =
      """
        |Unit.add = { |a, b| a + b }
        |
        |Unit.doThing = { |a, b = @add [@Unit, 1, 2]| a + b }
        |
        |@doThing [@Unit, 10]
        |""".stripMargin

    eval(code) shouldEqual 13
  }

  "Default arguments" should "be able to close over their outer scope" in {
    val code =
      """
        |@{
        |  id = { |x| x };
        |
        |  apply = { |val, fn = id| @fn [val] };
        |
        |  res = @apply [val = 1];
        |  res
        |}
        |""".stripMargin

    eval(code) shouldEqual 1
  }

  "Functions" should "use their default values when none is supplied" in {
    val code =
      """
        |Unit.addTogether = { |a = 5, b = 6| a + b }
        |
        |@addTogether [@Unit]
    """.stripMargin

    eval(code) shouldEqual 11
  }

  "Functions" should "override defaults by name" in {
    val code =
      """
        |Unit.addNum = { |a, num = 10| a + num }
        |
        |@addNum [@Unit, 1, num = 1]
    """.stripMargin

    eval(code) shouldEqual 2
  }

  "Functions" should "override defaults by position" in {
    val code =
      """
        |Unit.addNum = { |a, num = 10| a + num }
        |
        |@addNum [@Unit, 1, 2]
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Defaulted arguments" should "work in a recursive context" in {
    val code =
      """
        |Unit.summer = { |sumTo|
        |  summator = { |acc = 0, current|
        |      ifZero: [current, acc, @summator [current = current - 1, acc = acc + current]]
        |  };
        |  res = @summator [current = sumTo];
        |  res
        |}
        |
        |@summer [@Unit, 100]
    """.stripMargin

    eval(code) shouldEqual 5050
  }

  "Named Arguments" should "only be scoped to their definitions" in {
    val code =
      """
        |@{
        |  foo = { |x, y| x - y };
        |  bar = { |y, x| x - y };
        |
        |  baz = { |f| @f [x = 10, y = 11] };
        |
        |  a = @baz [foo];
        |  b = @baz [bar];
        |
        |  a - b
        |}
        |""".stripMargin

    eval(code) shouldEqual 0
  }

  "Named arguments" should "be applied in a sequence compatible with Eta-expansions" in {
    pending
    val code =
      """
        |Unit.foo = { |a, b, c| a + b }
        |@foo [@Unit, 20, a = 10]
        |""".stripMargin
  }

  "Default arguments" should "be able to depend on prior arguments" in {
    val code =
      """
        |Unit.doubleOrAdd = { |a, b = a| a + b }
        |
        |@doubleOrAdd [@Unit, 5]
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  "Default arguments" should "not be able to depend on later arguments" in {
    //TODO: Currently throws something equivalent to "Can't add dynamic symbol to Long". Needs rethinking.
    val code =
      """
        |Unit.badArgFn = { | a, b = c, c = a | (a + b) + c }
        |
        |@badArgFn [@Unit, 3]
        |""".stripMargin

    an[InterpreterException] should be thrownBy eval(code)
  }

  "Constructors" should "be able to use named arguments" in {
    val code =
      """
        |type Cons2 head rest;
        |type Nil2;
        |
        |@{
        |  genList = { |i| ifZero: [i, @Nil2, @Cons2 [rest = @genList [i-1], head = i]] };
        |
        |  sumList = { |list| match list <
        |    Cons2 ~ { |head, rest| head + @sumList [rest] };
        |    Nil2 ~ { 0 };
        |  >};
        |
        |  res = @sumList [@genList [10]];
        |  res
        |}
        """.stripMargin

    eval(code) shouldEqual 55
  }

  "Constructors" should "be able to take default arguments that are overridden" in {
    val code =
      """
        |type Nil2;
        |type Cons2 head (rest = Nil2);
        |@{
        |  genList = { |i| ifZero: [i, @Nil2, @Cons2 [rest = @genList [i-1], head = i]] };
        |
        |  sumList = { |list| match list <
        |    Cons2 ~ { |head, rest| head + @sumList [rest] };
        |    Nil2 ~ { 0 };
        |  >};
        |
        |  res = @sumList [@genList [5]];
        |  res
        |}
        """.stripMargin

    eval(code) shouldEqual 15
  }

  "Default arguments to constructors" should "be resolved dynamically" in {
    val code =
      """
        |type Cons2 head (rest = Nil2);
        |type Nil2;
        |
        |5
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  "Constructors" should "be able to take and use default arguments" in {
    val code =
      """
        |type Cons2 head (rest = @Nil2);
        |type Nil2;
        |
        |Unit.sumList = { |list| match list <
        |  Cons2 ~ { |head, rest| head + @sumList [@Unit, rest] };
        |  Nil2 ~ { 0 };
        |>}
        |
        |@sumList [@Unit, @Cons2 [10]]
        """.stripMargin

    eval(code) shouldEqual 10
  }

}
