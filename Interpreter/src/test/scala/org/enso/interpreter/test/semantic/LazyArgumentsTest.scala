package org.enso.interpreter.test.semantic

class LazyArgumentsTest extends LanguageTest {
  val subject = "Lazy arguments"

  subject should "not get executed upfront" in {
    val code =
      """
        |@{
        |  foo = { |i, $x, $y| ifZero: [i, $x, $y] };
        |  @foo [1, @println [@IO, 1], @println [@IO, 2]]
        |}
        |""".stripMargin
    noException should be thrownBy parse(code)
    eval(code)
    consumeOut shouldEqual List("2")
  }

  subject should "work well with tail recursion" in {
    val code =
      """
        |@{
        |  if = { |c, $ifT, $ifF| ifZero: [c, $ifT, $ifF] };
        |  sum = { |c, acc| @if [c, acc, @sum [c-1, acc + c]] };
        |  res = @sum [10000, 0];
        |  res
        |}
        |""".stripMargin
    eval(code) shouldEqual 50005000
  }

  subject should "work in non-tail positions" in {
    val code =
      """
        |@{
        |  suspInc = { |$x| 1 + ($x) };
        |  res = @suspInc [@suspInc [10]];
        |  res
        |}
        |""".stripMargin

    val result = eval(code)
    result shouldEqual 12
  }

  subject should "work properly with method dispatch" in {
    val code =
      """
        |type Foo;
        |type Bar;
        |
        |Foo.method = { |$x| 10 }
        |Bar.method = { |x| 10 }
        |
        |@{
        |  @method [@Foo, @println [@IO, 1]];
        |  @method [@Bar, @println [@IO, 2]];
        |  @method [@Foo, @println [@IO, 3]]
        |}
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("2")
  }

  subject should "work properly with oversaturated arguments" in {
    val code =
      """
        |@{
        |  if = { |c, $ifT, $ifF| ifZero: [c, $ifT, $ifF] };
        |  foo = { |c| @if [c] };
        |  @foo [0, @println [@IO, 1], @println [@IO, 2]];
        |  @foo [1, @println [@IO, 3], @println [@IO, 4]]
        |}
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("1","4")
  }
}
