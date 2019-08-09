package org.enso.interpreter.fixtures

import org.enso.interpreter.LanguageRunner

class AtomFixtures extends LanguageRunner {
  val million: Long        = 1000000

  val generateListCode =
    """
      |{ |length|
      |  generator = { |acc, i| ifZero: [i, acc, @generator [@Cons [i, acc], i - 1]] };
      |  res = @generator [@Nil, length];
      |  res
      |}
    """.stripMargin

  val generateList = eval(generateListCode)

  val millionElementList = generateList.call(million)

  val reverseListCode =
    """
      |{ |list|
      |  reverser = { |acc, list| match list <
      |    Cons ~ { |h, t| @reverser [@Cons [h, acc], t] };
      |    Nil  ~ { acc };
      |  >};
      |  res = @reverser [@Nil, list];
      |  res
      |}
    """.stripMargin

  val reverseList = eval(reverseListCode)

  val sumListCode =
    """
      |{ |list|
      |  sumator = { |acc, list| match list <
      |    Cons ~ { |h, t| @sumator [acc + h, t] };
      |    Nil ~ { acc };
      |  >};
      |  res = @sumator [0, list];
      |  res
      |}
    """.stripMargin

  val sumList = eval(sumListCode)

  val sumListFallbackCode =
    """
      |{ |list|
      |  sumator = { |acc, list| match list <
      |    Cons ~ { |h, t| @sumator [acc + h, t] };
      |    { acc };
      |  >};
      |  res = @sumator [0, list];
      |  res
      |}
    """.stripMargin

  val sumListFallback = eval(sumListFallbackCode)

}
