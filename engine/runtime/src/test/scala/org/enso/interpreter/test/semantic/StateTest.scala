package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class StateTest extends InterpreterTest {
  "State" should "be accessible from functions" in {
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

  "State" should "be implicitly threaded through function executions" in {
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

  "State" should "be localized with State.run" in {
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

  "State" should "work well with recursive code" in {
    val code =
      """
        |main =
        |    stateSum = n ->
        |        acc = State.get
        |        State.put acc+n
        |        ifZero n State.get (stateSum n-1)
        |
        |    State.run 0 (stateSum 10)
        |""".stripMargin
    eval(code) shouldEqual 55
  }

  "State" should "be initialized to a Unit by default" in {
    val code =
      """
        |main = IO.println State.get
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("Unit")
  }

  "State" should "work with pattern matches" in {
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

  "Panics" should "undo state changes" in {
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
