package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.DefaultInterpreterRunner

class RecursionFixtures extends DefaultInterpreterRunner {
  val hundredMillion: Long = 100000000
  val million: Long        = 1000000
  val thousand: Long       = 1000
  val hundred: Long        = 100

  val sumTCOCode =
    """import Standard.Base.Any.Any
      |
      |main = sumTo ->
      |    summator = acc -> current ->
      |        if current == 0 then acc else @Tail_Call summator acc+current current-1
      |
      |    res = summator 0 sumTo
      |    res
    """.stripMargin
  val sumTCO = getMain(sumTCOCode)

  val sumTCOFoldLikeCode =
    """import Standard.Base.Any.Any
      |
      |main = sumTo ->
      |    summator = acc -> i -> f ->
      |        if i == 0 then acc else @Tail_Call summator (f acc i) (i - 1) f
      |    res = summator 0 sumTo (x -> y -> x + y)
      |    res
      |""".stripMargin
  val sumTCOFoldLike = getMain(sumTCOFoldLikeCode)

  val sumRecursiveCode =
    """import Standard.Base.Any.Any
      |
      |main = sumTo ->
      |    summator = i -> if i == 0 then 0 else i + summator (i - 1)
      |    res = summator sumTo
      |    res
    """.stripMargin
  val sumRecursive = getMain(sumRecursiveCode)

  val oversaturatedRecursiveCallTCOCode =
    """import Standard.Base.Any.Any
      |
      |main = sumTo ->
      |    summator = acc -> i -> f ->
      |        if i == 0 then acc else @Tail_Call summator (f acc i) (i - 1) f
      |    res = summator 0 sumTo (x -> y -> x + y)
      |    res
      |""".stripMargin
  val oversaturatedRecursiveCall = getMain(oversaturatedRecursiveCallTCOCode)

  val sumStateTCOCode =
    """from Standard.Base.Data.Numbers import Number
      |import Standard.Base.Runtime.State
      |
      |stateSum = n ->
      |    acc = State.get Number
      |    State.put Number (acc + n)
      |    if n == 0 then State.get Number else @Tail_Call stateSum (n - 1)
      |
      |main = sumTo ->
      |    res = State.run Number 0 (stateSum sumTo)
      |    res
      |""".stripMargin
  val sumStateTCO = getMain(sumStateTCOCode)

  val sumTCOWithEvalCode =
    """import Standard.Base.Runtime.Debug
      |
      |main = sumTo ->
      |    summator = acc -> current ->
      |        if current == 0 then acc else Debug.eval "@Tail_Call summator (acc + current) (current - 1)"
      |
      |    res = summator 0 sumTo
      |    res
      |""".stripMargin
  val sumTCOWithEval = getMain(sumTCOWithEvalCode)

  val nestedThunkSumCode =
    """from Standard.Base.Data.Numbers import Number
      |import Standard.Base.Runtime.State
      |import Standard.Base.Nothing
      |
      |doNTimes = n -> ~block ->
      |    block
      |    if n == 1 then Nothing else @Tail_Call doNTimes n-1 block
      |
      |main = n ->
      |    block =
      |        x = State.get Number
      |        State.put Number x+1
      |
      |    res = State.run Number 0 (doNTimes n block)
      |    res
      |""".stripMargin
  val nestedThunkSum = getMain(nestedThunkSumCode)
}
