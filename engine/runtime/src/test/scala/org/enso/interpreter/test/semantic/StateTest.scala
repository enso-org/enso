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
          |    State.put 10
          |    x = State.get
          |    State.put x+1
          |    State.get
          |""".stripMargin

      eval(code) shouldEqual 11
    }

    "be implicitly threaded through function executions" in {
      val code =
        """
          |Unit.incState =
          |  x = State.get
          |  State.put x+1
          |
          |main =
          |    State.put 0
          |    Unit.incState
          |    Unit.incState
          |    Unit.incState
          |    Unit.incState
          |    Unit.incState
          |    State.get
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "be localized with State.run" in {
      val code =
        """
          |main =
          |    State.put 20
          |    myBlock =
          |        res = State.get
          |        State.put 0
          |        res
          |
          |    res2 = State.run 10 myBlock
          |    state = State.get
          |    res2 + state
          |""".stripMargin
      eval(code) shouldEqual 30
    }

    "work well with recursive code" in {
      val code =
        """
          |main =
          |    stateSum = n ->
          |        acc = State.get
          |        State.put acc+n
          |        if n == 0 then State.get else stateSum n-1
          |
          |    State.run 0 (stateSum 10)
          |""".stripMargin
      eval(code) shouldEqual 55
    }

    "be initialized to a Unit by default" in {
      val code =
        """
          |main = IO.println State.get
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("Unit")
    }

    "work with pattern matches" in {
      val code =
        """
          |main =
          |    matcher = x -> case x of
          |        Unit ->
          |            y = State.get
          |            State.put (y + 5)
          |        Nil ->
          |            y = State.get
          |            State.put (y + 10)
          |
          |    State.put 1
          |    matcher Nil
          |    IO.println State.get
          |    matcher Unit
          |    IO.println State.get
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
          |        State.put 400
          |        Panic.throw Unit
          |
          |    State.put 5
          |    Panic.recover panicker
          |    State.get
          |""".stripMargin
      eval(code) shouldEqual 5
    }
  }
}
