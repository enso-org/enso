package org.enso.interpreter.fixtures

import org.enso.interpreter.LanguageRunner

class AtomFixtures extends LanguageRunner {
  val million: Long = 1000000

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

  val reverseListMethodsCode =
    """
      |Cons.reverse = { |acc| match this <
      |  Cons ~ { |h, t| @reverse [t, @Cons [h, acc]] };
      |>}
      |
      |Nil.reverse = { |acc| acc }
      |
      |{ |list| @reverse [list, @Nil] }
      |""".stripMargin

  val reverseListMethods = eval(reverseListMethodsCode)

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

  val sumListMethodsCode =
    """
      |Nil.sum = { |acc| acc }
      |Cons.sum = { |acc| match this <
      |  Cons ~ { |h, t| @sum [t, h + acc] };
      |>}
      |
      |{ |list| @sum [list, 0] }
      |""".stripMargin

  val sumListMethods = eval(sumListMethodsCode)
}
