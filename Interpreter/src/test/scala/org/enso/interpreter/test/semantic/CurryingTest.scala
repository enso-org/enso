package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.LanguageTest

class CurryingTest extends LanguageTest {
  "Functions" should "allow partial application" in {
    val code =
      """
        |@{
        |  apply = { |v, f| @f [v] };
        |  adder = { |a, b| a + b };
        |  plusOne = @apply [f = @adder [1]];
        |  result = @plusOne [10];
        |  result
        |}
        |""".stripMargin
    eval(code) shouldEqual 11
  }

  "Functions" should "allow default arguments to be suspended" in {
    val code =
      """
        |@{
        |  fn = { |w, x, y = 10, z = 20| (w + x) + (y + z) };
        |
        |  fn1 = @fn ...;
        |  fn2 = @fn1 [1, 2] ...;
        |  fn3 = @fn2 [3] ...;
        |
        |  @fn3
        |}
        |""".stripMargin

    eval(code) shouldEqual 26
  }

  "Functions" should "allow defaults to be suspended in application chains" in {
    val code =
      """
        |@{
        |  fn = { |w, x, y = 10, z = 20| (w + x) + (y + z) };
        |
        |  @(@fn [3, 6] ...) [3]
        |}
        |""".stripMargin

    eval(code) shouldEqual 32
  }
}
