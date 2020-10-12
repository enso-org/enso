package org.enso.interpreter.test.instrument

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.polyglot.debugger.{DebugServerInfo, ObjectRepresentation}
import org.graalvm.polyglot.Context
import org.scalatest.{BeforeAndAfter, EitherValues}

class ReplTest extends InterpreterTest with BeforeAndAfter with EitherValues {

  override def subject: String = "Repl"

  override def contextModifiers: Context#Builder => Context#Builder =
    _.option(DebugServerInfo.ENABLE_OPTION, "true")

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "initialize properly" in {
      val code =
        """
          |main = Debug.breakpoint
          |""".stripMargin
      setSessionManager(executor => executor.exit())
      eval(code)
    }

    "be able to execute arbitrary code in the caller scope" in {
      val code =
        """
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

    "return the last evaluated value back to normal execution flow" in {
      val code =
        """
          |main =
          |    a = 5
          |    b = 6
          |    c = Debug.breakpoint
          |    c * a
          |""".stripMargin
      setSessionManager { executor =>
        executor.evaluate("a + b")
        executor.exit()
      }
      eval(code) shouldEqual 55
    }

    "be able to define its local variables" in {
      val code =
        """
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

    "not overwrite bindings" in {
      val code =
        """
          |main =
          |    x = 10
          |    Debug.breakpoint
          |    x
          |""".stripMargin
      setSessionManager { executor =>
        executor.evaluate("x = 20")
        executor.exit()
      }
      eval(code) shouldEqual 10
    }

    "access and modify monadic state" in {
      val code =
        """
          |run =
          |    State.put Number 10
          |    Debug.breakpoint
          |    State.get Number
          |
          |main = State.run Number 0 here.run
          |""".stripMargin
      setSessionManager { executor =>
        executor.evaluate("x = State.get Number")
        executor.evaluate("State.put Number (x + 1)")
        executor.exit()
      }
      eval(code) shouldEqual 11
    }

    "be able to list local variables in its scope" in {
      val code =
        """
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
        "this" -> "Test",
        "x"    -> "10",
        "y"    -> "20",
        "z"    -> "30"
      )
    }

    "be able to list bindings it has created" in {
      val code =
        """
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
        "this" -> "Test",
        "x"    -> "50",
        "y"    -> "20",
        "z"    -> "30"
      )
    }

    "allow to be nested" in {
      val code =
        """
          |main =
          |    10 * Debug.breakpoint + 1
          |""".stripMargin
      setSessionManager { topExecutor =>
        setSessionManager { nestedExecutor =>
          setSessionManager { doubleNestedExecutor =>
            doubleNestedExecutor.evaluate("4")
            doubleNestedExecutor.exit()
          }
          nestedExecutor.evaluate("10 * Debug.breakpoint + 3")
          nestedExecutor.exit()
        }
        topExecutor.evaluate("10 * Debug.breakpoint + 2")
        topExecutor.exit()
      }
      eval(code) shouldEqual 4321
    }

    "behave well when nested" in {
      val code =
        """
          |main =
          |    x = 1
          |    10 * Debug.breakpoint + x
          |""".stripMargin
      setSessionManager { topExecutor =>
        topExecutor.evaluate("x = 2")
        setSessionManager { nestedExecutor =>
          nestedExecutor.evaluate("x = 3")
          setSessionManager { doubleNestedExecutor =>
            doubleNestedExecutor.evaluate("x = 4")
            doubleNestedExecutor.evaluate("x")
            doubleNestedExecutor.exit()
          }
          nestedExecutor.evaluate("10 * Debug.breakpoint + x")
          nestedExecutor.exit()
        }
        topExecutor.evaluate("10 * Debug.breakpoint + x")
        topExecutor.exit()
      }
      eval(code) shouldEqual 4321
    }

    "handle errors gracefully" in {
      val code =
        """
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
        "Unexpected type provided for argument `that` in Number.+"
      evalResult.left.value.getMessage shouldEqual errorMsg
    }

    "attach language stack traces to the exception" in {
      val code =
        """
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

    "not pollute bindings upon nested error" in {
      val code =
        """
          |main =
          |    Debug.breakpoint
          |""".stripMargin
      var outerResult: Either[Exception, ObjectRepresentation] = null
      setSessionManager { outerExecutor =>
        outerExecutor.evaluate("x = \"outer\"")
        setSessionManager { innerExecutor =>
          innerExecutor.evaluate("x = \"inner\"")
          innerExecutor.exit()
        }

        // breakpoint will return Unit here, deliberately trigger an error
        outerExecutor.evaluate("1 + Debug.breakpoint")

        outerResult = outerExecutor.evaluate("x")
        outerExecutor.exit()
      }

      eval(code)
      outerResult.fold(_.toString, _.toString) shouldEqual "outer"
    }
  }
}
