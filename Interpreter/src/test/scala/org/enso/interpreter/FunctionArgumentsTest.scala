package org.enso.interpreter

class FunctionArgumentsTest extends LanguageTest {
  "Functions" should "take arguments and use them in their bodies" in {
    val code     = "{ |x| x * x }"
    val function = eval(code)
    function.call(1) shouldEqual 1
    function.call(4) shouldEqual 16
  }

  "Function arguments from outer scope" should "be visible in the inner scope" in {
    val code =
      """
        |{ |a|
        |  adder = { |b| a + b };
        |  res = @adder [2];
        |  res
        |}  
      """.stripMargin

    eval(code).call(3) shouldEqual 5
  }

  "Recursion" should "work" in {
    val code =
      """
        |@{
        |  sumTo = { |x| ifZero: [x, 0, x + (@sumTo [x - 1])] };
        |  @sumTo [10]
        |}
      """.stripMargin

    eval(code) shouldEqual 55
  }

  "Function calls" should "accept more arguments than needed and pass them to the result upon execution" in {
    val code =
      """
        |@{
        |  f = { |x| { |z| x + z } };
        |
        |  @f [1, 2]
        |}
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Function calls" should "allow oversaturation and execute until completion" in {
    val code =
      """
        |@{
        |  f = { | x, y | { | w | { | z | (x * y) + (w + z) } } };
        |
        |  @f [3, 3, 10, 1]
        |}
        |""".stripMargin

    eval(code) shouldEqual 20
  }

  "Function calls" should "be able to return atoms that are evaluated with oversaturated args" in {
    val code =
      """
        |@{
        |  f = { |x| Cons };
        |
        |  myCons = @f [1, 2, 3];
        |
        |  match myCons <
        |    Cons ~ { |h, t| h + t };
        |  >
        |}
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  "Methods" should "support the use of oversaturated args" in {
    val code =
      """
        |Unit.myMethod = 1
        |
        |@{
        |  f = { |x| myMethod };
        |  t = @f [10, @Unit];
        |
        |  t
        |}
        |""".stripMargin

    eval(code) shouldEqual 1
  }
}
