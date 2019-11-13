package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class StateTest extends InterpreterTest {
  "State" should "be accessible from functions" in {
    val code =
      """
        |@{
        |  @put [@State, 10];
        |  x = @get [@State];
        |  @put [@State, x + 1];
        |  @get [@State]
        |}
        |""".stripMargin

    eval(code) shouldEqual 11
  }

  "State" should "be implicitly threaded through function executions" in {
    val code =
      """
        |Unit.incState = {
        |  x = @get [@State];
        |  @put [@State, x + 1]
        |}
        |
        |@{
        |  @put [@State, 0];
        |  @incState [@Unit];
        |  @incState [@Unit];
        |  @incState [@Unit];
        |  @incState [@Unit];
        |  @incState [@Unit];
        |  @get [@State]
        |}
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  "State" should "be localized with State.run" in {
    val code =
      """
        |@{
        |  @put[@State, 20];
        |  myFun = {
        |    res = @get[@State];
        |    @put[@State, 0];
        |    res
        |  };
        |  res = @run[@State, 10, @myFun];
        |  state = @get[@State];
        |  res + state
        |}
        |""".stripMargin
    eval(code) shouldEqual 30
  }

  "State" should "work well with recursive code" in {
    val code =
      """
        |@{
        |  stateSum = { |n|
        |    acc = @get [@State];
        |    @println[@IO, acc];
        |    @put [@State, acc + n];
        |    @ifZero [n, @get [@State], @stateSum [n-1]]
        |  };
        |  @run [@State, 0, @stateSum [10]]
        |}
        |""".stripMargin
    eval(code) shouldEqual 55
  }

  "State" should "be initialized to a Unit by default" in {
    val code =
      """
        |@println[@IO, @get[@State]]
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("Unit<>")
  }

  "State" should "work with pattern matches" in {
    val code =
      """
        |@{
        |  matcher = { |x| match x <
        |    Unit ~ { y = @get[@State]; @put[@State, y+5] };
        |    Nil ~ { y = @get[@State]; @put[@State, y+10] };
        |  >};
        |  @put[@State, 1];
        |  @matcher[@Nil];
        |  @println[@IO, @get[@State]];
        |  @matcher[@Unit];
        |  @println[@IO, @get[@State]];
        |  0
        |}
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("11", "16")
  }

  "Panics" should "undo state changes" in {
    val code =
      """
        |@{
        |  panicker = { @put[@State, 400]; @throw[@Panic, @Unit] };
        |  @put[@State,-5];
        |  @recover[@Panic, @panicker];
        |  @get[@State]
        |}
        |""".stripMargin
    eval(code) shouldEqual (-5)
  }
}
