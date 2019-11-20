package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.InterpreterRunner

class AtomFixtures extends InterpreterRunner {
  val million: Long = 1000000

  val generateListCode =
    """
      |{ |length|
      |  generator = { |acc, i| @ifZero [i, acc, @generator [@Cons [i, acc], i - 1]] };
      |  res = @generator [@Nil, length];
      |  res
      |}
    """.stripMargin

  val generateList = evalOld(generateListCode)

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

  val reverseList = evalOld(reverseListCode)

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

  val reverseListMethods = evalOld(reverseListMethodsCode)

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

  val sumList = evalOld(sumListCode)

  val sumListLeftFoldCode =
    """
      |{ |list|
      |  fold = { |f, acc, list| match list <
      |    Cons ~ { |h, t| @fold [f, @f[acc, h], t] };
      |    { acc };
      |  >};
      |
      |  res = @fold [{ |x, y| x + y }, 0, list];
      |  res
      |}
    """.stripMargin

  val sumListLeftFold = evalOld(sumListLeftFoldCode)

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

  val sumListFallback = evalOld(sumListFallbackCode)

  val sumListMethodsCode =
    """
      |Nil.sum = { |acc| acc }
      |Cons.sum = { |acc| match this <
      |  Cons ~ { |h, t| @sum [t, h + acc] };
      |>}
      |
      |{ |list| @sum [list, 0] }
      |""".stripMargin

  val sumListMethods = evalOld(sumListMethodsCode)

  val mapReverseListCode =
    """
      |Nil.mapReverse = { |f, acc| acc }
      |Cons.mapReverse = { |f, acc| match this <
      |  Cons ~ { |h, t| @mapReverse [t, f, @Cons [@f [h], acc]] };
      |>}
      |
      |{ |list| @mapReverse [list, { |x| x + 1 }, @Nil] }
      |""".stripMargin

  val mapReverseList = evalOld(mapReverseListCode)

  val mapReverseListCurryCode =
    """
      |Nil.mapReverse = { |f, acc| acc }
      |Cons.mapReverse = { |f, acc| match this <
      |  Cons ~ { |h, t| @mapReverse [t, f, @Cons [@f [h], acc]] };
      |>}
      |
      |{ |list|
      |  adder = { |x, y| x + y };
      |  @mapReverse [list, @adder [1], @Nil]
      |}
      |""".stripMargin

  val mapReverseListCurry = evalOld(mapReverseListCurryCode)
}
