package org.enso.interpreter.test.instrument

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.polyglot.debugger.{DebugServerInfo, ObjectRepresentation}
import org.graalvm.polyglot.Context
import org.scalatest.{BeforeAndAfter, EitherValues, Inside}

class DebugServerTest
    extends InterpreterTest
    with BeforeAndAfter
    with EitherValues
    with Inside {

  override def subject: String = "Repl"

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(b => {
      b.option(DebugServerInfo.FN_OPTION, "main")
    })

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "initialize properly" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |
          |main = Debug.breakpoint
          |""".stripMargin
      setSessionManager(executor => executor.exit())
      eval(code)
    }

    "be able to execute arbitrary code in the caller scope" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |import Standard.Base.Data.Numbers
          |
          |main =
          |    x = 1
          |    y = 2
          |    Debug.breakpoint
          |""".stripMargin
      var evalResult: Either[Exception, ObjectRepresentation] =
        null
      setSessionManager { executor =>
        evalResult = executor.evaluate("x + y")
        executor.exit()
      }
      eval(code) shouldEqual 3
      evalResult.fold(_.toString, _.toString) shouldEqual "3"
    }

    "be able to define its local variables" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |
          |main =
          |    x = 10
          |    Debug.breakpoint
          |""".stripMargin
      setSessionManager { executor =>
        executor.evaluate("y = x + 1")
        executor.evaluate("z = y * x")
        executor.evaluate("z")
        executor.exit()
      }
      eval(code) shouldEqual 110
    }

    "be able to list local variables in its scope" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |
          |main =
          |    x = 10
          |    y = 20
          |    z = x + y
          |
          |    Debug.breakpoint
          |""".stripMargin
      var scopeResult: Map[String, ObjectRepresentation] = Map()
      setSessionManager { executor =>
        scopeResult = executor.listBindings()
        executor.exit()
      }
      eval(code)
      scopeResult.view.mapValues(_.toString).toMap shouldEqual Map(
        "x" -> "10",
        "y" -> "20",
        "z" -> "30"
      )
    }

    "be able to list bindings it has created" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |
          |main =
          |    x = 10
          |    y = 20
          |    z = x + y
          |
          |    Debug.breakpoint
          |""".stripMargin
      var scopeResult: Map[String, ObjectRepresentation] = Map()
      setSessionManager { executor =>
        executor.evaluate("x = y + z")
        scopeResult = executor.listBindings()
        executor.exit()
      }
      eval(code)
      scopeResult.view.mapValues(_.toString).toMap shouldEqual Map(
        "x" -> "50",
        "y" -> "20",
        "z" -> "30"
      )
    }

    "handle errors gracefully" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |
          |main =
          |    Debug.breakpoint
          |""".stripMargin
      var evalResult: Either[Exception, ObjectRepresentation] =
        null
      setSessionManager { executor =>
        evalResult = executor.evaluate("1 + undefined")
        executor.exit()
      }
      eval(code)
      val errorMsg =
        "Compile_Error.Error"
      evalResult.left.value.getMessage shouldEqual errorMsg
    }

    "handle errors gracefully (pretty print)" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |from Standard.Base.Errors.Common import all
          |
          |main =
          |    Debug.breakpoint
          |""".stripMargin
      var evalResult: Either[Exception, ObjectRepresentation] =
        null
      setSessionManager { executor =>
        evalResult = executor.evaluate("1 + undefined")
        executor.exit()
      }
      eval(code)
      val errorMsg =
        "Compile error: The name `undefined` could not be found."
      evalResult.left.value.getMessage shouldEqual errorMsg
    }

    "attach language stack traces to the exception" in {
      val code =
        """
          |import Standard.Base.Runtime.Debug
          |import Standard.Base.Panic.Panic
          |
          |main =
          |    Debug.breakpoint
          |""".stripMargin
      var evalResult: Either[Exception, ObjectRepresentation] =
        null
      setSessionManager { executor =>
        evalResult = executor.evaluate("Panic.throw \"Panic\"")
        executor.exit()
      }
      eval(code)

      var lastException: Throwable = evalResult.left.value
      while (lastException.getCause != null) {
        lastException = lastException.getCause
      }

      val traceMethodNames = lastException.getStackTrace.map(_.getMethodName)
      traceMethodNames should contain("Panic.throw")
      traceMethodNames should contain("Debug.breakpoint")
    }
  }
}
