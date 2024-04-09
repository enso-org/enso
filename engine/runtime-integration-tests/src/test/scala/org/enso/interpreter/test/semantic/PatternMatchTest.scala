package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class PatternMatchTest extends InterpreterTest {

  override def subject = "Pattern Matching"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "work for simple patterns" in {
      val code =
        """import Standard.Base.Data.List.List
          |
          |main =
          |    f = case _ of
          |        List.Cons a _ -> a
          |        List.Nil -> -10
          |
          |    f (List.Cons 10 List.Nil) - f List.Nil
          |""".stripMargin

      eval(code) shouldEqual 20
    }

    "work for anonymous catch-all patterns" in {
      val code =
        """import Standard.Base.Data.List.List
          |
          |type My_Atom
          |    Mk a
          |
          |main =
          |    f = case _ of
          |        My_Atom.Mk a -> a
          |        _ -> -100
          |
          |    f (My_Atom.Mk 50) + f List.Nil
          |""".stripMargin

      eval(code) shouldEqual -50
    }

    "work for named catch-all patterns" in {
      val code =
        """
          |type My_Atom
          |    Mk a
          |
          |main =
          |    f = case _ of
          |        My_Atom.Mk a -> a
          |        a -> a + 5
          |
          |    f (My_Atom.Mk 50) + f 30
          |""".stripMargin

      eval(code) shouldEqual 85
    }

    "work without assignment" in {
      val code =
        """
          |type MyAtom
          |
          |main = case MyAtom of
          |    MyAtom -> 10
          |    _ -> - 10
          |""".stripMargin

      eval(code) shouldEqual 10
    }

    "work for level one nested patterns" in {
      val code =
        """import Standard.Base.Data.List.List
          |
          |type MyAtom
          |
          |main =
          |    f = case _ of
          |        List.Cons MyAtom _ -> 30
          |        _ -> -30
          |
          |    f (List.Cons MyAtom List.Nil)
          |""".stripMargin

      eval(code) shouldEqual 30
    }

    "work for deeply nested patterns" in {
      val code =
        """import Standard.Base.Data.List.List
          |
          |type MyAtom
          |
          |main =
          |    f = case _ of
          |        List.Cons (List.Cons MyAtom List.Nil) List.Nil -> 100
          |        List.Cons _ List.Nil -> 50
          |        y -> case y of
          |            List.Cons _ List.Nil -> 30
          |            _ -> 0
          |
          |    val1 = f (List.Cons MyAtom List.Nil)                      # 50
          |    val2 = f (List.Cons (List.Cons MyAtom List.Nil) List.Nil) # 100
          |    val3 = f 40                                               # 0
          |
          |    val1 + val2 + val3
          |""".stripMargin

      eval(code) shouldEqual 150
    }

    "correctly result in errors for incomplete matches" in {
      val code =
        """import Standard.Base.Data.List.List
          |
          |type MyAtom
          |
          |main =
          |    f = case _ of
          |        List.Nil -> 30
          |
          |    f MyAtom
          |""".stripMargin

      val msg = "Inexhaustive pattern match: no branch matches MyAtom."
      the[InterpreterException] thrownBy eval(code) should have message msg
    }

    "work for pattern matches in pattern matches" in {
      val code =
        """import Standard.Base.Data.List.List
          |
          |type My_Atom
          |    Mk a
          |type One
          |    Mk a
          |type Two
          |    Mk a
          |
          |main =
          |    f = case _ of
          |        My_Atom.Mk a -> case a of
          |            One.Mk List.Nil -> 50
          |            _ -> 30
          |        _ -> 20
          |
          |    f (My_Atom.Mk (One.Mk List.Nil))
          |""".stripMargin

      eval(code) shouldEqual 50
    }
  }
}
