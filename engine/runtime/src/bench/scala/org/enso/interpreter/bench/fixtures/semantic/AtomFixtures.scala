package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.DefaultInterpreterRunner
import org.enso.interpreter.runtime.builtin.Builtins
import org.graalvm.polyglot.Value

class AtomFixtures extends DefaultInterpreterRunner {
  val million: Long = 1000000

  def buildInputList(length: Long): Value = {
    val builtins =
      interpreterContext.executionContext.getTopScope
        .getModule(Builtins.MODULE_NAME)
    val nil  = builtins.getConstructor("Nil")
    val cons = builtins.getConstructor("Cons")
    1L.to(length).foldLeft(nil.newInstance()) { case (tail, el) =>
      cons.newInstance(el.asInstanceOf[Object], tail)
    }
  }
  val millionElementList = buildInputList(million)

  val generateListCode =
    """from Standard.Builtins import all
      |
      |main = length ->
      |    generator = acc -> i -> if i == 0 then acc else @Tail_Call generator (Cons i acc) (i - 1)
      |
      |    res = generator Nil length
      |    res
    """.stripMargin
  val generateList = getMain(generateListCode)

  val generateListQualifiedCode =
    """from Standard.Builtins import all
      |
      |main = length ->
      |    generator = acc -> i -> if i == 0 then acc else @Tail_Call generator (Builtins.cons i acc) (i - 1)
      |
      |    res = generator Builtins.nil length
      |    res
    """.stripMargin
  val generateListQualified = getMain(generateListQualifiedCode)

  val reverseListCode =
    """from Standard.Builtins import all
      |
      |main = list ->
      |    reverser = acc -> list -> case list of
      |        Cons h t -> @Tail_Call reverser (Cons h acc) t
      |        Nil -> acc
      |
      |    res = reverser Nil list
      |    res
    """.stripMargin
  val reverseList = getMain(reverseListCode)

  val reverseListMethodsCode =
    """from Standard.Builtins import all
      |
      |Cons.reverse = acc -> case this of
      |    Cons h t -> @Tail_Call t.reverse (Cons h acc)
      |
      |Nil.reverse = acc -> acc
      |
      |main = list ->
      |    res = list.reverse Nil
      |    res
      |""".stripMargin
  val reverseListMethods = getMain(reverseListMethodsCode)

  val sumListCode =
    """from Standard.Builtins import all
      |
      |main = list ->
      |    summator = acc -> list -> case list of
      |        Cons h t -> @Tail_Call summator acc+h t
      |        Nil -> acc
      |
      |    res = summator 0 list
      |    res
    """.stripMargin
  val sumList = getMain(sumListCode)

  val sumListLeftFoldCode =
    """from Standard.Builtins import all
      |
      |main = list ->
      |    fold = f -> acc -> list -> case list of
      |        Cons h t -> @Tail_Call fold f (f acc h) t
      |        _ -> acc
      |
      |    res = fold (x -> y -> x + y) 0 list
      |    res
    """.stripMargin
  val sumListLeftFold = getMain(sumListLeftFoldCode)

  val sumListFallbackCode =
    """from Standard.Builtins import all
      |
      |main = list ->
      |    summator = acc -> list -> case list of
      |        Cons h t -> @Tail_Call summator acc+h t
      |        _ -> acc
      |
      |    res = summator 0 list
      |    res
    """.stripMargin
  val sumListFallback = getMain(sumListFallbackCode)

  val sumListMethodsCode =
    """from Standard.Builtins import all
      |
      |Nil.sum = acc -> acc
      |Cons.sum = acc -> case this of
      |    Cons h t -> @Tail_Call t.sum h+acc
      |
      |main = list ->
      |    res = list.sum 0
      |    res
      |""".stripMargin
  val sumListMethods = getMain(sumListMethodsCode)

  val mapReverseListCode =
    """from Standard.Builtins import all
      |
      |Nil.mapReverse = f -> acc -> acc
      |Cons.mapReverse = f -> acc -> case this of
      |    Cons h t -> @Tail_Call t.mapReverse f (Cons (f h) acc)
      |
      |main = list ->
      |    res = list.mapReverse (x -> x + 1) Nil
      |    res
      |""".stripMargin
  val mapReverseList = getMain(mapReverseListCode)

  val mapReverseListCurryCode =
    """from Standard.Builtins import all
      |
      |Nil.mapReverse = f -> acc -> acc
      |Cons.mapReverse = f -> acc -> case this of
      |    Cons h t -> @Tail_Call t.mapReverse f (Cons (f h) acc)
      |
      |main = list ->
      |    adder = x -> y -> x + y
      |    res = list.mapReverse (adder 1) Nil
      |    res
      |""".stripMargin
  val mapReverseListCurry = getMain(mapReverseListCurryCode)
}
