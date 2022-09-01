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
        """from Standard.Base.Data.List import all
          |
          |main =
          |    f = case _ of
          |        Cons a _ -> a
          |        Nil -> -10
          |
          |    f (Cons 10 Nil) - f Nil
          |""".stripMargin

      eval(code) shouldEqual 20
    }

    "work for anonymous catch-all patterns" in {
      val code =
        """from Standard.Base.Data.List import all
          |
          |type My_Atom
          |    Mk_My_Atom a
          |
          |main =
          |    f = case _ of
          |        Mk_My_Atom a -> a
          |        _ -> -100
          |
          |    f (Mk_My_Atom 50) + f Nil
          |""".stripMargin

      eval(code) shouldEqual -50
    }

    "work for named catch-all patterns" in {
      val code =
        """
          |type My_Atom
          |    Mk_My_Atom a
          |
          |main =
          |    f = case _ of
          |        Mk_My_Atom a -> a
          |        a -> a + 5
          |
          |    f (Mk_My_Atom 50) + f 30
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
        """from Standard.Base.Data.List import all
          |
          |type MyAtom
          |
          |main =
          |    f = case _ of
          |        Cons MyAtom _ -> 30
          |        _ -> -30
          |
          |    f (Cons MyAtom Nil)
          |""".stripMargin

      eval(code) shouldEqual 30
    }

    "work for deeply nested patterns" in {
      val code =
        """from Standard.Base.Data.List import all
          |
          |type MyAtom
          |
          |main =
          |    f = case _ of
          |        Cons (Cons MyAtom Nil) Nil -> 100
          |        Cons _ Nil -> 50
          |        y -> case y of
          |            Cons _ Nil -> 30
          |            _ -> 0
          |
          |    val1 = f (Cons MyAtom Nil)            # 50
          |    val2 = f (Cons (Cons MyAtom Nil) Nil) # 100
          |    val3 = f 40                           # 0
          |
          |    val1 + val2 + val3
          |""".stripMargin

      eval(code) shouldEqual 150
    }

    "correctly result in errors for incomplete matches" in {
      val code =
        """from Standard.Base.Data.List import all
          |
          |type MyAtom
          |
          |main =
          |    f = case _ of
          |        Nil -> 30
          |
          |    f MyAtom
          |""".stripMargin

      val msg = "Inexhaustive pattern match: no branch matches MyAtom."
      the[InterpreterException] thrownBy eval(code) should have message msg
    }

    "work for pattern matches in pattern matches" in {
      val code =
        """from Standard.Base.Data.List import all
          |
          |type My_Atom
          |    Mk_My_Atom a
          |type One
          |    Mk_One a
          |type Two
          |    Mk_Two a
          |
          |main =
          |    f = case _ of
          |        Mk_My_Atom a -> case a of
          |            Mk_One Nil -> 50
          |            _ -> 30
          |        _ -> 20
          |
          |    f (Mk_My_Atom (Mk_One Nil))
          |""".stripMargin

      eval(code) shouldEqual 50
    }
  }
}
