package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class StateTest extends InterpreterTest {
  "State" should "be accessible from functions" in {
    val code =
      """
        |State.put 10
        |x = State.get
        |State.put x+1
        |State.get
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
        |State.put 0
        |Unit.incState
        |Unit.incState
        |Unit.incState
        |Unit.incState
        |Unit.incState
        |State.get
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  // TODO [AA,MK]: New syntax must support suspended blocks like `myFun` here
  "State" should "be localized with State.run" in {
    val code =
      """
        |@{
        |  @put[State, 20];
        |  myFun = {
        |    res = @get[State];
        |    @put[State, 0];
        |    res
        |  };
        |  res = @run[State, 10, @myFun];
        |  state = @get[State];
        |  res + state
        |}
        |""".stripMargin
    evalOld(code) shouldEqual 30
  }

  "State" should "work well with recursive code" in {
    val code =
      """
        |stateSum = n ->
        |  acc = State.get
        |  State.put acc+n
        |  ifZero n State.get (stateSum n-1)
        |
        |State.run 0 (stateSum 10)
        |""".stripMargin
    eval(code) shouldEqual 55
  }

  "State" should "be initialized to a Unit by default" in {
    val code =
      """
        |IO.println State.get
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("Unit<>")
  }

  "State" should "work with pattern matches" in {
    val code =
      """
        |matcher = x -> case x of
        |  Unit ->
        |    y = State.get
        |    State.put (y + 5)
        |  Nil ->
        |    y = State.get
        |    State.put (y + 10)
        |
        |State.put 1
        |matcher Nil
        |IO.println State.get
        |matcher Unit
        |IO.println State.get
        |0
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("11", "16")
  }

  // TODO [AA] Needs suspended blocks
  "Panics" should "undo state changes" in {
    val code =
      """
        |@{
        |  panicker = { @put[State, 400]; @throw[Panic, Unit] };
        |  @put[State,-5];
        |  @recover[Panic, @panicker];
        |  @get[State]
        |}
        |""".stripMargin
    evalOld(code) shouldEqual (-5)
  }
}
