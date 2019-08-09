package org.enso.interpreter

import org.graalvm.polyglot.PolyglotException

class NamedArgumentsTest extends LanguageTest {
  "Functions" should "take arguments by name and use them in their bodies" in {

    val code =
      """
        |a = 10
        |addTen = { |b| a + b }
        |
        |@addTen [b = 10]
      """.stripMargin

    eval(code) shouldEqual 20
  }

  "Functions" should "be able to have named arguments given out of order" in {
    val code =
      """
        |subtract = { |a, b| a - b }
        |
        |@subtract [b = 10, a = 5]
    """.stripMargin

    eval(code) shouldEqual -5
  }

  "Functions" should "be able to have scope values as named arguments" in {
    val code =
      """
        |a = 10
        |addTen = { |num| num + a }
        |
        |@addTen [num = a]
    """.stripMargin

    eval(code) shouldEqual 20
  }

  "Functions" should "be able to be defined with default argument values" in {
    val code =
      """
        |addNum = { |a, num = 10| a + num }
        |
        |@addNum [5]
    """.stripMargin

    eval(code) shouldEqual 15
  }

  "Default arguments" should "be able to default to complex expressions" in {
    val code =
      """
        |add = { |a, b| a + b }
        |
        |doThing = { |a, b = @add [1, 2]| a + b }
        |
        |@doThing [10]
        |""".stripMargin

    eval(code) shouldEqual 13
  }

  "Default arguments" should "be able to close over their outer scope" in {
    val code =
      """
        |id = { |x| x }
        |
        |apply = { |val, fn = id| @id [val] }
        |
        |@apply [val = 1]
        |""".stripMargin

    eval(code) shouldEqual 1
  }

  "Functions" should "use their default values when none is supplied" in {
    val code =
      """
        |addTogether = { |a = 5, b = 6| a + b }
        |
        |@addTogether
    """.stripMargin

    eval(code) shouldEqual 11
  }

  "Functions" should "override defaults by name" in {
    val code =
      """
        |addNum = { |a, num = 10| a + num }
        |
        |@addNum [1, num = 1]
    """.stripMargin

    eval(code) shouldEqual 2
  }

  "Functions" should "override defaults by position" in {
    val code =
      """
        |addNum = { |a, num = 10| a + num }
        |
        |@addNum [1, 2]
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Defaulted arguments" should "work in a recursive context" in {
    val code =
      """
        |summer = { |sumTo|
        |  summator = { |acc = 0, current|
        |      ifZero: [current, acc, @summator [current = current - 1, acc = acc + current]]
        |  };
        |  res = @summator [current = sumTo];
        |  res
        |}
        |
        |@summer [100]
    """.stripMargin

    eval(code) shouldEqual 5050
  }

  "Named Arguments" should "only be scoped to their definitions" in {
    val code =
      """
        |foo = { |x, y| x - y }
        |bar = { |y, x| x - y }
        |
        |baz = { |f| @f [x = 10, y = 11] }
        |
        |a = @baz [foo]
        |b = @baz [bar]
        |
        |a - b
        |""".stripMargin

    eval(code) shouldEqual 0
  }

  "Named arguments" should "be applied in a sequence compatible with Eta-expansions" in {
    pending
    val code =
      """
        |foo = { |a, b, c| a + b }
        |@foo [20, a = 10]
        |""".stripMargin
  }

  "Default arguments" should "be able to depend on prior arguments" in {
    val code =
      """
        |doubleOrAdd = { |a, b = a| a + b }
        |
        |@doubleOrAdd [5]
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  "Default arguments" should "not be able to depend on later arguments" in {
    val code =
      """
        |badArgFn = { | a, b = c, c = a | a + b + c }
        |
        |@badArgFn [3]
        |""".stripMargin

    val errMsg = "java.lang.RuntimeException: No result when parsing failed"

    the[PolyglotException] thrownBy eval(code) should have message errMsg
  }

  "Constructors" should "be able to use named arguments" in {
    val code =
      """
        |type Cons2 head rest;
        |type Nil2;
        |
        |genList = { |i| ifZero: [i, @Nil2, @Cons2 [rest = @genList [i-1], head = i]] }
        |
        |sumList = { |list| match list <
        |  Cons2 ~ { |head, rest| head + @sumList [rest] };
        |  Nil2 ~ { 0 };
        |>}
        |
        |@sumList [@genList [10]]
        """.stripMargin

    eval(code) shouldEqual 55
  }

  "Constructors" should "be able to take default arguments that are overridden" in {
    val code =
      """
        |type Nil2;
        |type Cons2 head (rest = Nil2);
        |
        |genList = { |i| ifZero: [i, @Nil2, @Cons2 [rest = @genList [i-1], head = i]] }
        |
        |sumList = { |list| match list <
        |  Cons2 ~ { |head, rest| head + @sumList [rest] };
        |  Nil2 ~ { 0 };
        |>}
        |
        |@sumList [@genList [5]]
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

    pending
    eval(code) shouldEqual 5
  }

  "Constructors" should "be able to take and use default arguments" in {
    pending
    val code =
      """
        |type Nil2;
        |type Cons2 head (rest = Nil2);
        |
        |sumList = { |list| match list <
        |  Cons2 ~ { |head, rest| head + @sumList [rest] };
        |  Nil2 ~ { 0 };
        |>}
        |
        |@sumList [@Cons2 [10]]
        """.stripMargin

    eval(code) shouldEqual 10
  }

  "Constructor arguments" should "be matchable in arbitrary order by name" in {
    val code =
      """
        |type Nil2;
        |type Cons2 head (rest = Nil2);
        |
        |genList = { |i| ifZero: [i, @Nil2, @Cons2 [rest = @genList [i-1], head = i]] }
        |
        |sumList = { |list| match list <
        |  Cons2 ~ { |rest, head| head + @sumList [rest] };
        |  Nil2 ~ { 0 };
        |>}
        |
        |@sumList [@genList [5]]
        """.stripMargin

    pending
    eval(code) shouldEqual 15
  }

}
