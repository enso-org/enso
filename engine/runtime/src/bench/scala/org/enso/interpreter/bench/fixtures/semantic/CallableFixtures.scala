package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.DefaultInterpreterRunner

class CallableFixtures extends DefaultInterpreterRunner {
  val hundredMillion: Long = 100000000

  val sumTCOfromCallCode =
    """
      |from Standard.Base.Data.Numbers import all
      |
      |type Foo
      |
      |Foo.from (that : Number) current=0 =
      |    if current == 0 then that else @Tail_Call Foo.from (that + current) (current - 1)
      |
      |main = sumTo ->
      |    res = Foo.from 0 sumTo
      |    res
      |""".stripMargin
  val sumTCOfromCall = getMain(sumTCOfromCallCode)


  val sumTCOmethodCallCode =
    """
      |summator = acc -> current ->
      |    if current == 0 then acc else @Tail_Call here.summator (acc + current) (current - 1)
      |
      |main = sumTo ->
      |    res = here.summator 0 sumTo
      |    res
      |""".stripMargin
  val sumTCOmethodCall = getMain(sumTCOmethodCallCode)

  val sumTCOmethodCallWithNamedArgumentsCode =
    """
      |summator = acc -> current ->
      |    if current == 0 then acc else @Tail_Call here.summator (current = current - 1) (acc = acc + current)
      |
      |main = sumTo ->
      |    res = here.summator current=sumTo acc=0
      |    res
      |""".stripMargin
  val sumTCOmethodCallWithNamedArguments =
    getMain(sumTCOmethodCallWithNamedArgumentsCode)

  val sumTCOmethodCallWithDefaultedArgumentsCode =
    """
      |summator = (acc = 0) -> current ->
      |    if current == 0 then acc else @Tail_Call here.summator (current = current - 1) (acc = acc + current)
      |
      |main = sumTo ->
      |    res = here.summator current=sumTo
      |    res
      |""".stripMargin
  val sumTCOmethodCallWithDefaultedArguments =
    getMain(sumTCOmethodCallWithDefaultedArgumentsCode)

}
