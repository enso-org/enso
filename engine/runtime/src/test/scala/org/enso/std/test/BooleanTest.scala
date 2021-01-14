package org.enso.std.test

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class BooleanTest extends InterpreterTest {
  override def subject = "Booleans"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "support if_then_else" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    if True then IO.println "true when true" else IO.println "false when true"
          |    if False then IO.println "true when false" else IO.println "false when false"
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("true when true", "false when false")
    }

    "support overriding methods on boolean" in {
      val code =
        """from Builtins import all
          |
          |Boolean.isTrue = this
          |
          |main =
          |    true = 1 == 1
          |    false = 1 == 2
          |    IO.println true.isTrue
          |    IO.println false.isTrue
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("True", "False")
    }

    "support pattern matching" in {
      val code =
        """from Builtins import all
          |
          |to_num b = case b of
          |    True -> 1
          |    False -> 2
          |    _ -> 10
          |
          |main =
          |    here.to_num True + here.to_num False
          |""".stripMargin
      eval(code) shouldEqual 3
    }

    "support per-constructor method overloads" in {
      val code =
        """from Builtins import all
          |
          |True.to_num = 1
          |False.to_num = 2
          |
          |main = True.to_num + False.to_num
          |""".stripMargin
      eval(code) shouldEqual 3
    }

    "support per-single-constructor method overloads" in {
      val code =
        """from Builtins import all
          |
          |Boolean.to_num = 2
          |True.to_num = 1
          |
          |main = True.to_num + False.to_num
          |""".stripMargin
      eval(code) shouldEqual 3
    }

    "support logical AND and OR operators" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    IO.println True&&False
          |    IO.println True&&True
          |    IO.println False||False
          |    IO.println True||False
          |    IO.println ((True && False) || (True && True))
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("False", "True", "False", "True", "True")
    }

    "support negation" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    IO.println True.not
          |    IO.println False.not
          |    IO.println (1==2 . not)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("False", "True", "True")
    }
  }
}
