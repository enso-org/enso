package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class StateTest extends InterpreterTest {
  override def subject: String = "State"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "be accessible from functions" in {
      val code =
        """from Builtins import all
          |
          |stateful =
          |    State.put Number 10
          |    x = State.get Number
          |    State.put Number x+1
          |    State.get Number
          |
          |main = State.run Number 0 here.stateful
          |""".stripMargin

      eval(code) shouldEqual 11
    }

    "be implicitly threaded through function executions" in {
      val code =
        """from Builtins import all
          |
          |inc_state =
          |  x = State.get Number
          |  State.put Number x+1
          |
          |run =
          |    here.inc_state
          |    here.inc_state
          |    here.inc_state
          |    here.inc_state
          |    here.inc_state
          |    State.get Number
          |
          |main = State.run Number 0 here.run
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "work well with recursive code" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    stateSum = n ->
          |        acc = State.get Number
          |        State.put Number acc+n
          |        if n == 0 then State.get Number else stateSum n-1
          |
          |    State.run Number 0 (stateSum 10)
          |""".stripMargin
      eval(code) shouldEqual 55
    }

    "work with pattern matches" in {
      val code =
        """from Builtins import all
          |
          |run =
          |    matcher = x -> case x of
          |        Nothing ->
          |            y = State.get Number
          |            State.put Number (y + 5)
          |        Nil ->
          |            y = State.get Number
          |            State.put Number (y + 10)
          |
          |    State.put Number 1
          |    matcher Nil
          |    IO.println (State.get Number)
          |    matcher Nothing
          |    IO.println (State.get Number)
          |    0
          |
          |main = State.run Number 0 here.run
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("11", "16")
    }

    "undo changes on Panics" in {
      val code =
        """from Builtins import all
          |
          |panicker =
          |    State.put Number 400
          |    Panic.throw Nothing
          |
          |stater =
          |    State.put Number 5
          |    Panic.recover here.panicker
          |    State.get Number
          |
          |main = State.run Number 0 here.stater
          |""".stripMargin
      eval(code) shouldEqual 5
    }

    "localize properly with State.run when 1 key used" in {
      val code =
        """from Builtins import all
          |
          |inner = State.put Number 0
          |
          |outer =
          |    State.put Number 1
          |    State.run Number 2 here.inner
          |    State.get Number
          |
          |main = State.run Number 3 here.outer
          |""".stripMargin
      eval(code) shouldEqual 1
    }

    "localize properly with State.run when 2 states used" in {
      val code =
        """from Builtins import all
          |
          |type S1
          |type S2
          |
          |inner =
          |    State.put S1 0
          |    State.put S2 0
          |
          |outer =
          |    State.put S1 1
          |    State.run S2 2 here.inner
          |    State.get S1
          |
          |main = State.run S1 3 here.outer
          |
          |""".stripMargin
      eval(code) shouldEqual 0
    }

    "localize properly with State.run when multiple states used" in {
      val code =
        """from Builtins import all
          |
          |type S1
          |type S2
          |type S3
          |
          |inner =
          |    State.put S1 0
          |    State.put S2 0
          |
          |outer =
          |    State.put S1 1
          |    State.put S3 2
          |    State.run S2 2 here.inner
          |    State.get S1 + State.get S2 + State.get S3
          |
          |main = State.run S3 0 (State.run S2 5 (State.run S1 3 here.outer))
          |
          |""".stripMargin
      eval(code) shouldEqual 7 // S1 = 0, S2 = 5, S3 = 2
    }
  }
}
