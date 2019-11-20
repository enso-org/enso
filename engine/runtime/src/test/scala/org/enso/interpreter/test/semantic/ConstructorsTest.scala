package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}

class ConstructorsTest extends InterpreterTest {

  "Pattern matching" should "dispatch to the proper branch" in {
    val patternMatchingCode =
      """
        |@{
        |  x = @Cons [1, @Nil];
        |  match x <
        |    Cons ~ { |h, t| h };
        |    Nil  ~ { 0 };
        |  >
        |}  
      """.stripMargin
    evalOld(patternMatchingCode) shouldEqual 1
  }

  "Recursion with pattern matching" should "terminate" in {
    val testCode =
      """
        |@{
        |  genList = { |i| @ifZero [i, @Nil, @Cons [i, @genList [i-1]]] };
        |  sumList = { |list| match list <
        |    Cons ~ { |h, t| h + (@sumList [t]) };
        |    Nil  ~ { 0 };
        |  >};
        |  res = @sumList [@genList [10]];
        |  res
        |}
      """.stripMargin
    evalOld(testCode) shouldEqual 55
  }

  "Pattern match expression" should "behave correctly in non-tail positions" in {
    val testCode =
      """
        |{
        |  add = { |x, y| x + y };
        |  testCons = @Cons [1,2];
        |  result = match testCons <
        |    Cons ~ { |x, y| @add [x,y] };
        |  >;
        |  result + 1
        |}
      """.stripMargin
    evalOld(testCode).execute() shouldEqual 4
  }

  "Pattern match expressions" should "accept a catch-all fallback clause" in {
    val testCode =
      """
        |{
        |  nil = @Nil;
        |  match nil <
        |    Cons ~ { 0 };
        |    { 1 };
        |  >
        |}
      """.stripMargin
    evalOld(testCode).execute() shouldEqual 1
  }

  "Pattern match expressions" should "throw an exception when match fails" in {
    val testCode =
      """
        |{
        |  nil = @Nil;
        |  match nil <
        |    Cons ~ { 0 };
        |  >
        |}
      """.stripMargin
    the[InterpreterException] thrownBy evalOld(testCode)
      .call() should have message "Inexhaustive pattern match."
  }

  "Constructor definitions" should "be usable in code, with arbitrary definition order" in {
    val testCode =
      """
        |type Cons2 a b;
        |
        |Unit.genList = { |i| @ifZero [i, @Nil2, @Cons2 [i, @genList [@Unit, i-1]]] }
        |
        |type Nil2;
        |
        |Unit.sumList = { |list| match list <
        |  Cons2 ~ { |a, b| a + @sumList [@Unit, b] };
        |  Nil2 ~ { 0 };
        |>}
        |
        |@sumList [@Unit, @genList [@Unit, 10]]
      """.stripMargin
    evalOld(testCode) shouldEqual 55
  }
}
