package org.enso.interpreter

class RecursionFixtures extends LanguageRunner {
  val hundredMillion: Long = 100000000

  // Currently unused as we know this is very slow.
  val mutRecursiveCode =
    """
    |summator = { |acc, current|
    |    ifZero: [current, acc, @summator [acc + current, current - 1]]
    |}
    |
    |{ |sumTo|
    |  res = @summator [0, sumTo];
    |  res
    |}
    |"""

  val sumTCOCode =
    """
      |{ |sumTo|
      |  summator = { |acc, current|
      |      ifZero: [current, acc, @summator [acc + current, current - 1]]
      |  };
      |  res = @summator [0, sumTo];
      |  res
      |}
    """.stripMargin

  val sumTCO = ctx.eval(Constants.LANGUAGE_ID, sumTCOCode)

  val sumRecursiveCode =
    """
      |{ |sumTo|
      |  summator = { |i| ifZero: [i, 0, i + (@summator [i - 1])] };
      |  res = @summator [sumTo];
      |  res
      |}
    """.stripMargin

  val sumRecursive = ctx.eval(Constants.LANGUAGE_ID, sumRecursiveCode)
}
