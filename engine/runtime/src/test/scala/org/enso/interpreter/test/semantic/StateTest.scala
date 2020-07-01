package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class StateTest extends InterpreterTest {
  override def subject: String = "State"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "be accessible from functions" in {
      val code =
        """
          |main =
          |    State.put Number 10
          |    x = State.get Number
          |    State.put Number x+1
          |    State.get Number
          |""".stripMargin

      eval(code) shouldEqual 11
    }

    "be implicitly threaded through function executions" in {
      val code =
        """
          |Unit.incState =
          |  x = State.get Number
          |  State.put Number x+1
          |
          |main =
          |    State.put Number 0
          |    Unit.incState
          |    Unit.incState
          |    Unit.incState
          |    Unit.incState
          |    Unit.incState
          |    State.get Number
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "be localized with State.run" in {
      val code =
        """
          |main =
          |    State.put Number 20
          |    myBlock =
          |        res = State.get Number
          |        State.put Number 0
          |        res
          |
          |    res2 = State.run Number 10 myBlock
          |    state = State.get Number
          |    res2 + state
          |""".stripMargin
      eval(code) shouldEqual 30
    }

    "work well with recursive code" in {
      val code =
        """
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

//    "be initialized to a Unit by default" in {
//      val code =
//        """
//          |main = IO.println (State.get Number)
//          |""".stripMargin
//      eval(code)
//      consumeOut shouldEqual List("Unit")
//    }

    "work with pattern matches" in {
      val code =
        """
          |main =
          |    matcher = x -> case x of
          |        Unit ->
          |            y = State.get Number
          |            State.put Number (y + 5)
          |        Nil ->
          |            y = State.get
          |            State.put Number (y + 10)
          |
          |    State.put Number 1
          |    matcher Nil
          |    IO.println (State.get Number)
          |    matcher Unit
          |    IO.println (State.get Number)
          |    0
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("11", "16")
    }

    "undo changes on Panics" in {
      val code =
        """
          |main =
          |    panicker =
          |        State.put Number 400
          |        Panic.throw Unit
          |
          |    State.put Number 5
          |    Panic.recover panicker
          |    State.get Number
          |""".stripMargin
      eval(code) shouldEqual 5
    }
  }
}
