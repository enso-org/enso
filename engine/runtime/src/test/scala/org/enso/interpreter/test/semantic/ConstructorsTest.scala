package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class ConstructorsTest extends InterpreterTest {

  override def subject: String = "Constructors & Pattern Matching"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "dispatch to the proper match branch" in {
      val patternMatchingCode =
        """import Standard.Base.Data.List.List
          |
          |main =
          |    x = List.Cons 1 List.Nil
          |    case x of
          |        List.Cons h t -> h
          |        List.Nil -> 0
          |""".stripMargin
      eval(patternMatchingCode) shouldEqual 1
    }

    "work with recursion" in {
      val testCode =
        """import Standard.Base.Data.List.List
          |import Standard.Base.Any.Any
          |
          |main =
          |    genList = i -> if i == 0 then List.Nil else List.Cons i (genList (i - 1))
          |    sumList = list -> case list of
          |        List.Cons h t -> h + sumList t
          |        List.Nil -> 0
          |
          |    sumList (genList 10)
          |""".stripMargin
      eval(testCode) shouldEqual 55
    }

    "behave correctly in non-tail positions" in {
      val testCode =
        """import Standard.Base.Data.List.List
          |
          |main =
          |    add = x -> y -> x + y
          |    testCons = List.Cons 1 2
          |
          |    result = case testCons of
          |        List.Cons x y -> add x y
          |
          |    result + 1
          |""".stripMargin
      eval(testCode) shouldEqual 4
    }

    "accept a catch-all fallback clause" in {
      val testCode =
        """import Standard.Base.Data.List.List
          |
          |main =
          |    nil = List.Nil
          |    case nil of
          |        List.Cons _ _ -> 0
          |        _ -> 1
          |""".stripMargin
      eval(testCode) shouldEqual 1
    }

    "throw an exception when match fails" in {
      val testCode =
        """import Standard.Base.Data.List.List
          |
          |main =
          |    nil = List.Nil
          |    case nil of
          |        List.Cons h t -> 0
          |""".stripMargin
      the[InterpreterException] thrownBy eval(testCode)
        .call() should have message "Inexhaustive pattern match: no branch matches Nil."
    }

    "be usable in code, with arbitrary definition order" in {
      val testCode =
        """import Standard.Base.Nothing.Nothing
          |import Standard.Base.Any.Any
          |
          |type C2
          |    Cons2 a b
          |
          |Nothing.genList = i -> if i == 0 then Nil2 else C2.Cons2 i (Nothing.genList (i - 1))
          |
          |type Nil2
          |
          |Nothing.sumList = list -> case list of
          |  C2.Cons2 h t -> h + Nothing.sumList t
          |  Nil2 -> 0
          |
          |main = Nothing.sumList (Nothing.genList 10)
          |""".stripMargin
      eval(testCode) shouldEqual 55
    }
  }
}
