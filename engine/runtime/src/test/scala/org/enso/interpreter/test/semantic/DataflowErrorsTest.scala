package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class DataflowErrorsTest extends InterpreterTest {
  override def subject: String = "Dataflow Errors"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "propagate through pattern matches" in {
      val code =
        """from Builtins import all
          |
          |type MyError
          |
          |main =
          |    brokenVal = Error.throw MyError
          |    matched = case brokenVal of
          |        Nothing -> 1
          |        _ -> 0
          |
          |    IO.println matched
          |""".stripMargin
      noException shouldBe thrownBy(eval(code))
      consumeOut shouldEqual List("(Error: MyError)")
    }

    "propagate through specialized pattern matches" in {
      val code =
        """from Builtins import all
          |
          |type MyError
          |
          |main =
          |    brokenVal = Error.throw MyError
          |    f = case _ of
          |        Nothing -> 1
          |        _ -> 0
          |
          |    IO.println (f Nothing)
          |    IO.println (f brokenVal)
          |""".stripMargin
      noException shouldBe thrownBy(eval(code))
      consumeOut shouldEqual List("1", "(Error: MyError)")
    }

    "be catchable by a user-provided special handling function" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    intError = Error.throw 1
          |    intError.catch_primitive (x -> x + 3)
          |""".stripMargin
      eval(code) shouldEqual 4
    }

    "accept a constructor handler in catch function" in {
      val code =
        """from Builtins import all
          |
          |type MyCons err
          |
          |main =
          |    unitErr = Error.throw Nothing
          |    IO.println (unitErr.catch_primitive MyCons)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(MyCons Nothing)")
    }

    "accept a method handle in catch function" in {
      val code =
        """from Builtins import all
          |
          |type MyRecovered x
          |type MyError x
          |
          |MyError.recover = case this of
          |    MyError x -> MyRecovered x
          |
          |main =
          |    myErr = Error.throw (MyError 20)
          |    IO.println(myErr.catch_primitive .recover)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(MyRecovered 20)")
    }

    "make the catch method an identity for non-error values" in {
      val code = "main = 10.catch_primitive (x -> x + 1)"
      eval(code) shouldEqual 10
    }

    "propagate through atom construction" in {
      val code =
        """from Builtins import all
          |
          |type My_Atom a
          |type My_Error
          |
          |main =
          |    broken_val = Error.throw My_Error
          |    atom = My_Atom broken_val
          |
          |    IO.println atom
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(Error: My_Error)")
    }

    "propagate through method resolution" in {
      val code =
        """from Builtins import all
          |
          |type My_Atom
          |type My_Error
          |
          |My_Atom.foo = 10
          |
          |main =
          |    broken_val = Error.throw My_Error
          |    result = broken_val.foo
          |
          |    IO.println result
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(Error: My_Error)")
    }

    "propagate through function calls" in {
      val code =
        """from Builtins import all
          |
          |type My_Error
          |
          |main =
          |    fn = Error.throw My_Error
          |    result = fn 1 2
          |    IO.println result
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("(Error: My_Error)")
    }

    "propagate through builtin methods" in {
      val code =
        """from Builtins import all
          |
          |type My_Error
          |
          |main =
          |    result = 1 + (Error.throw My_Error)
          |    IO.println result
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("(Error: My_Error)")
    }

    "not propagate when explicitly accepted by type and by annotation" in {
      val code =
        """from Builtins import all
          |
          |type My_Error
          |
          |main =
          |    text = Error.throw My_Error . to_text
          |    IO.println text
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("(Error: My_Error)")
    }
  }
}
