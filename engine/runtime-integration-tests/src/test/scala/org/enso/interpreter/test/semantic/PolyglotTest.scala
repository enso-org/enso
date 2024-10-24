package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class PolyglotTest extends InterpreterTest {
  override def subject: String = "Polyglot"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "allow calling methods on static objects" in {
      val code =
        """from Standard.Base import all
          |import Standard.Base.Data.Array.Array
          |polyglot java import org.enso.example.TestClass
          |
          |main =
          |    class = TestClass
          |    method = Polyglot.get_member class "add"
          |    Polyglot.execute method ([1, 2].to_array)
          |""".stripMargin

      eval(code) shouldEqual 3
    }

    "interop exception without a message" in {
      val code =
        """import Standard.Base.IO
          |polyglot java import org.enso.example.TestClass
          |main =
          |    IO.println <| TestClass.raiseException 0
          |""".stripMargin

      try {
        eval(code) shouldEqual "An exception shall be thrown"
      } catch {
        case ex: InterpreterException =>
          ex.getMessage() shouldEqual null
      }
    }
    "interop exception with a message" in {
      val code =
        """import Standard.Base.IO
          |polyglot java import org.enso.example.TestClass
          |main =
          |    IO.println <| TestClass.raiseException 1
          |""".stripMargin

      try {
        eval(code) shouldEqual "An exception shall be thrown"
      } catch {
        case ex: InterpreterException => ex.getMessage() shouldEqual "NPE!"
      }
    }

    "allow instantiating objects and calling methods on them" in {
      val code =
        """from Standard.Base import all
          |polyglot java import org.enso.example.TestClass
          |
          |main =
          |    class = TestClass
          |    instance = Polyglot.new class [x -> x * 2]
          |    Polyglot.invoke instance "callFunctionAndIncrement" [10]
          |""".stripMargin
      eval(code) shouldEqual 21
    }

    "allow listing available members of an object" in {
      val code =
        """from Standard.Base import all
          |polyglot java import org.enso.example.TestClass
          |
          |main =
          |    class = TestClass
          |    instance = Polyglot.new class []
          |    members = Polyglot.get_members instance
          |    IO.println members.length
          |    IO.println (members.at 0)
          |    IO.println (members.at 1)
          |    IO.println (members.at 2)
          |""".stripMargin
      eval(code)
      val count :: methods = consumeOut
      count shouldEqual "3"
      methods.toSet shouldEqual Set(
        "method1",
        "method2",
        "callFunctionAndIncrement"
      )
    }

    "empty members when message not supported" in {
      val code =
        """from Standard.Base import all
          |
          |main =
          |    instance = "Hi There"
          |    members = Polyglot.get_members instance
          |    IO.println members.length
          |    IO.println members
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("0", "[]")
    }

    "fail to match on Polyglot symbol when imported everything from stdlib" in {
      val code =
        """from Standard.Base import all
          |import Standard.Base.Polyglot as Polyglot_Module
          |polyglot java import java.util.Random
          |
          |main =
          |    random_gen = Random.new
          |    case random_gen of
          |        Polyglot_Module -> IO.println "OK"
          |        _ -> IO.println "FAIL"
          |""".stripMargin
      eval(code)
      val count :: Nil = consumeOut
      count shouldEqual "FAIL"
    }

    "match on Polyglot type when explicitly importing everything from Polyglot module" in {
      val code =
        """import Standard.Base.Polyglot.Polyglot
          |import Standard.Base.IO
          |polyglot java import java.util.Random
          |
          |main =
          |    random_gen = Random.new
          |    case random_gen of
          |        Polyglot -> IO.println "OK"
          |        _ -> IO.println "FAIL"
          |""".stripMargin
      eval(code)
      val count :: Nil = consumeOut
      count shouldEqual "OK"
    }

    "fail to match on Polyglot type case when only importing Polyglot module" in {
      val code =
        """import Standard.Base.Polyglot
          |import Standard.Base.IO
          |polyglot java import java.util.Random
          |
          |main =
          |    random_gen = Random.new
          |    case random_gen of
          |        Polyglot -> IO.println "OK"
          |        _ -> IO.println "FAIL"
          |""".stripMargin
      eval(code)
      val count :: Nil = consumeOut
      count shouldEqual "FAIL"
    }

    "match on qualified name of the Polyglot type from Polyglot module" in {
      val code =
        """import Standard.Base.Polyglot
          |import Standard.Base.IO
          |polyglot java import java.util.Random
          |
          |main =
          |    random_gen = Random.new
          |    case random_gen of
          |        Polyglot.Polyglot -> IO.println "OK"
          |        _ -> IO.println "FAIL"
          |""".stripMargin
      eval(code)
      val count :: Nil = consumeOut
      count shouldEqual "OK"
    }

  }
}
