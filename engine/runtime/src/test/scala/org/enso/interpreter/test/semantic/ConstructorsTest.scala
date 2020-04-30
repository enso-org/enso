package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}

class ConstructorsTest extends InterpreterTest {

  "Pattern matching" should "dispatch to the proper branch" in {
    val patternMatchingCode =
      """
        |main =
        |    x = Cons 1 Nil
        |    case x of
        |        Cons h t -> h
        |        Nil -> 0
      """.stripMargin
    eval(patternMatchingCode) shouldEqual 1
  }

  "Recursion with pattern matching" should "terminate" in {
    val testCode =
      """
        |main =
        |    genList = i -> ifZero i Nil (Cons i (genList (i - 1)))
        |    sumList = list -> case list of
        |        Cons h t -> h + sumList t
        |        Nil -> 0
        |
        |    sumList (genList 10)
      """.stripMargin
    eval(testCode) shouldEqual 55
  }

  "Pattern match expression" should "behave correctly in non-tail positions" in {
    val testCode =
      """
        |main =
        |    add = x -> y -> x + y
        |    testCons = Cons 1 2
        |
        |    result = case testCons of
        |        Cons x y -> add x y
        |
        |    result + 1
      """.stripMargin
    eval(testCode) shouldEqual 4
  }

  "Pattern match expressions" should "accept a catch-all fallback clause" in {
    // TODO [AA] Once we can desugar `_` ignores for pattern matches they should
    //  be used here.
    val testCode =
      """
        |main =
        |    nil = Nil
        |    case nil of
        |        Cons h t -> 0
        |        _ -> 1
      """.stripMargin
    eval(testCode) shouldEqual 1
  }

  "Pattern match expressions" should "throw an exception when match fails" in {
    val testCode =
      """
        |main =
        |    nil = Nil
        |    case nil of
        |        Cons h t -> 0
      """.stripMargin
    the[InterpreterException] thrownBy eval(testCode)
      .call() should have message "Inexhaustive pattern match: the Nil case is not handled."
  }

  "Constructor definitions" should "be usable in code, with arbitrary definition order" in {
    val testCode =
      """
        |type Cons2 a b
        |
        |Unit.genList = i -> ifZero i Nil2 (Cons2 i (genList Unit (i - 1)))
        |
        |type Nil2
        |
        |Unit.sumList = list -> case list of
        |  Cons2 h t -> h + sumList Unit t
        |  Nil2 -> 0
        |
        |main = sumList Unit (genList Unit 10)
      """.stripMargin
    eval(testCode) shouldEqual 55
  }
}
