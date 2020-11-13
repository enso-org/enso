package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.DefaultInterpreterRunner

class NamedDefaultedArgumentFixtures extends DefaultInterpreterRunner {
  val hundredMillion: Long = 100000000

  val sumTCOWithNamedArgumentsCode =
    """
      |main = sumTo ->
      |    summator = acc -> current ->
      |        if current == 0 then acc else summator (current = current - 1) (acc = acc + current)
      |
      |    res = summator current=sumTo acc=0
      |    res
    """.stripMargin
  val sumTCOWithNamedArguments = getMain(sumTCOWithNamedArgumentsCode)

  val sumTCOWithDefaultedArgumentsCode =
    """
      |main = sumTo ->
      |    summator = (acc = 0) -> current ->
      |        if current == 0 then acc else summator (current = current - 1) (acc = acc + current)
      |
      |    res = summator (current = sumTo)
      |    res
    """.stripMargin
  val sumTCOWithDefaultedArguments = getMain(sumTCOWithDefaultedArgumentsCode)

}
