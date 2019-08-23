package org.enso.interpreter;

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
}
