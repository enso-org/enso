package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.InterpreterRunner

class RecursionFixtures extends InterpreterRunner {
  val hundredMillion: Long = 100000000
  val million: Long        = 1000000
  val thousand: Long       = 1000
  val hundred: Long        = 100

  val sumTCOCode =
    """
      |main = sumTo ->
      |    summator = acc current ->
      |        ifZero current acc (summator acc+current current-1)
      |
      |    res = summator 0 sumTo
      |    res
    """.stripMargin
  val sumTCO = getMain(sumTCOCode)

  val sumTCOFoldLikeCode =
    """
      |main = sumTo ->
      |    summator = acc i f ->
      |        ifZero i acc (summator (f acc i) (i - 1) f)
      |    res = summator 0 sumTo (x y -> x + y)
      |    res
      |""".stripMargin
  val sumTCOFoldLike = getMain(sumTCOFoldLikeCode)

  val sumRecursiveCode =
    """
      |main = sumTo ->
      |    summator = i -> ifZero i 0 (i + summator (i - 1))
      |    res = summator sumTo
      |    res
    """.stripMargin
  val sumRecursive = getMain(sumRecursiveCode)

  val oversaturatedRecursiveCallTCOCode =
    """
      |main = sumTo ->
      |    summator = acc i f ->
      |        ifZero i acc (summator (f acc i) (i - 1) f)
      |    res = summator 0 sumTo (x -> y -> x + y)
      |    res
      |""".stripMargin
  val oversaturatedRecursiveCall = getMain(oversaturatedRecursiveCallTCOCode)

  val sumStateTCOCode =
    """
      |main = sumTo ->
      |    stateSum = n ->
      |        acc = State.get
      |        State.put (acc + n)
      |        ifZero n State.get (stateSum (n - 1))
      |
      |    State.put 0
      |    res = stateSum sumTo
      |    res
      |""".stripMargin
  val sumStateTCO = getMain(sumStateTCOCode)

  val sumTCOWithEvalCode =
    """
      |main = sumTo ->
      |    summator = acc current ->
      |        ifZero current acc (Debug.eval "summator (acc + current) (current - 1)")
      |
      |    res = summator 0 sumTo
      |    res
      |""".stripMargin
  val sumTCOWithEval = getMain(sumTCOWithEvalCode)

  val nestedThunkSumCode =
    """
      |main = n ->
      |    doNTimes = n ~block ->
      |        block
      |        ifZero n-1 Unit (doNTimes n-1 block)
      |
      |    block =
      |        x = State.get
      |        State.put x+1
      |
      |    State.put 0
      |    doNTimes n block
      |    State.get
      |""".stripMargin
  val nestedThunkSum = getMain(nestedThunkSumCode)
}
