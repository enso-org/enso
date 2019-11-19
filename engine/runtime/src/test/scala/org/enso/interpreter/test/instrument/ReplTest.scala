package org.enso.interpreter.test.instrument

import org.enso.interpreter.test.InterpreterTest
import collection.JavaConverters._

class ReplTest extends InterpreterTest {
  "Repl" should "be able to list local variables in its scope" in {
    val code =
      """
        |@{
        |  x = 10;
        |  y = 20;
        |  z = x + y;
        |  @breakpoint[@Debug]
        |}
        |""".stripMargin
    var scopeResult: Map[String, AnyRef] = Map()
    getReplInstrument.setSessionManager { executor =>
      scopeResult = executor.listBindings.asScala.toMap
      executor.exit()
    }
    eval(code)
    scopeResult shouldEqual Map("x" -> 10, "y" -> 20, "z" -> 30)
  }

  "Repl" should "be able to execute arbitrary code in the caller scope" in {
    val code =
      """
        |@{
        |  x = 1;
        |  y = 2;
        |  @breakpoint[@Debug]
        |}
        |""".stripMargin
    var evalResult: AnyRef = null
    getReplInstrument.setSessionManager { executor =>
      evalResult = executor.evaluate("x + y")
      executor.exit()
    }
    eval(code)
    evalResult shouldEqual 3
  }

  "Repl" should "return the last evaluated value back to normal execution flow" in {
    val code =
      """
        |@{
        |  a = 5;
        |  b = 6;
        |  c = @breakpoint[@Debug];
        |  c * a
        |}
        |""".stripMargin
    getReplInstrument.setSessionManager { executor =>
      executor.evaluate("a + b")
      executor.exit()
    }
    eval(code) shouldEqual 55
  }

  "Repl" should "be able to define its local variables" in {
    val code =
      """
        |@{
        |  x = 10;
        |  @breakpoint[@Debug]
        |}
        |""".stripMargin
    getReplInstrument.setSessionManager { executor =>
      executor.evaluate("y = x + 1")
      executor.evaluate("z = y * x")
      executor.evaluate("z")
      executor.exit()
    }
    eval(code) shouldEqual 110
  }

  "Repl" should "access and modify monadic state" in {
    val code =
      """
        |@{
        |  @put[@State, 10];
        |  @breakpoint[@Debug];
        |  @get[@State]
        |}
        |""".stripMargin
    getReplInstrument.setSessionManager { executor =>
      executor.evaluate("x = @get[@State]")
      executor.evaluate("@put[@State, x+1]")
      executor.exit()
    }
    eval(code) shouldEqual 11
  }
}
