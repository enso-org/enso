package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.InterpreterRunner

class RecursionFixtures extends InterpreterRunner {
  val hundredMillion: Long = 100000000
  val million: Long        = 1000000
  val thousand: Long       = 1000
  val hundred: Long        = 100

  val sumTCOCode =
    """
      |sumTo ->
      |  summator = acc current ->
      |    ifZero current acc (summator acc+current current-1)
      |
      |  res = summator 0 sumTo
      |  res
    """.stripMargin
  val sumTCO = eval(sumTCOCode)

  val sumTCOFoldLikeCode =
    """
      |sumTo ->
      |  summator = acc i f ->
      |    ifZero i acc (summator (f acc i) (i - 1) f)
      |  res = summator 0 sumTo (x y -> x + y)
      |  res
      |""".stripMargin
  val sumTCOFoldLike = eval(sumTCOFoldLikeCode)

  val sumRecursiveCode =
    """
      |sumTo ->
      |  summator = i -> ifZero i 0 (i + summator (i - 1))
      |
      |  res = summator sumTo
      |  res
    """.stripMargin
  val sumRecursive = eval(sumRecursiveCode)

  val oversaturatedRecursiveCallTCOCode =
    """
      |sumTo ->
      |  summator = acc i f ->
      |    ifZero i acc (summator (f acc i) (i - 1) f)
      |  res = summator 0 sumTo (x -> y -> x + y)
      |  res
      |""".stripMargin
  val oversaturatedRecursiveCall = eval(oversaturatedRecursiveCallTCOCode)

  val sumStateTCOCode =
    """
      |sumTo ->
      |  stateSum = n ->
      |    acc = State.get
      |    State.put (acc + n)
      |    ifZero n State.get (stateSum (n - 1))
      |
      |  State.put 0
      |  stateSum sumTo
      |""".stripMargin
  val sumStateTCO = eval(sumStateTCOCode)

  val sumTCOWithEvalCode =
    """
      |sumTo ->
      |  summator = acc current ->
      |    ifZero current acc (Debug.eval "summator (acc + current) (current - 1)")
      |
      |  res = summator 0 sumTo
      |  res
      |""".stripMargin
  val sumTCOWithEval = eval(sumTCOWithEvalCode)

  val nestedThunkSumCode =
    """
      |n ->
      |  doNTimes = n ~block ->
      |    ~block
      |    ifZero n-1 Unit (doNTimes n-1 ~block)
      |
      |  block =
      |    x = State.get
      |    State.put x+1
      |
      |  State.put 0
      |  doNTimes n ~block
      |  State.get
      |""".stripMargin
  val nestedThunkSum = eval(nestedThunkSumCode)
}
