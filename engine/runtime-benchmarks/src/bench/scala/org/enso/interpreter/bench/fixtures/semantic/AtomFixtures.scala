package org.enso.interpreter.bench.fixtures.semantic

import org.enso.interpreter.test.DefaultInterpreterRunner

class AtomFixtures extends DefaultInterpreterRunner {
  val million: Long = 1000000

  val millionElementList = eval(
    s"""
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main =
      |    generator fn acc i end = if i == end then acc else @Tail_Call generator fn (fn acc i) i+1 end
      |    res = generator (acc -> x -> List.Cons x acc) List.Nil 1 $million
      |    res
      |""".stripMargin)

  val generateListCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main = length ->
      |    generator = acc -> i -> if i == 0 then acc else @Tail_Call generator (List.Cons i acc) (i - 1)
      |
      |    res = generator List.Nil length
      |    res
      |""".stripMargin
  val generateList = getMain(generateListCode)

  val generateListQualifiedCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main = length ->
      |    generator = acc -> i -> if i == 0 then acc else @Tail_Call generator (List.Cons i acc) (i - 1)
      |
      |    res = generator List.Nil length
      |    res
      |""".stripMargin
  val generateListQualified = getMain(generateListQualifiedCode)

  val reverseListCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main = list ->
      |    reverser = acc -> list -> case list of
      |        List.Cons h t -> @Tail_Call reverser (List.Cons h acc) t
      |        List.Nil -> acc
      |
      |    res = reverser List.Nil list
      |    res
      |""".stripMargin
  val reverseList = getMain(reverseListCode)

  val reverseListMethodsCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |List.rev self acc = case self of
      |    List.Cons h t -> @Tail_Call t.rev (List.Cons h acc)
      |    _ -> acc
      |
      |main = list ->
      |    res = list.rev List.Nil
      |    res
      |""".stripMargin
  val reverseListMethods = getMain(reverseListMethodsCode)

  val sumListCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main = list ->
      |    summator = acc -> list -> case list of
      |        List.Cons h t -> @Tail_Call summator acc+h t
      |        List.Nil -> acc
      |
      |    res = summator 0 list
      |    res
      |""".stripMargin
  val sumList = getMain(sumListCode)

  val sumListLeftFoldCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main = list ->
      |    fold = f -> acc -> list -> case list of
      |        List.Cons h t -> @Tail_Call fold f (f acc h) t
      |        _ -> acc
      |
      |    res = fold (x -> y -> x + y) 0 list
      |    res
      |""".stripMargin
  val sumListLeftFold = getMain(sumListLeftFoldCode)

  val sumListFallbackCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |main = list ->
      |    summator = acc -> list -> case list of
      |        List.Cons h t -> @Tail_Call summator acc+h t
      |        _ -> acc
      |
      |    res = summator 0 list
      |    res
      |""".stripMargin
  val sumListFallback = getMain(sumListFallbackCode)

  val sumListMethodsCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |List.sum self acc = case self of
      |    List.Cons h t -> @Tail_Call t.sum h+acc
      |    _ -> acc
      |
      |main = list ->
      |    res = list.sum 0
      |    res
      |""".stripMargin
  val sumListMethods = getMain(sumListMethodsCode)

  val mapReverseListCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |List.mapReverse self f acc = case self of
      |    List.Cons h t -> @Tail_Call t.mapReverse f (List.Cons (f h) acc)
      |    _ -> acc
      |
      |main = list ->
      |    res = list.mapReverse (x -> x + 1) List.Nil
      |    res
      |""".stripMargin
  val mapReverseList = getMain(mapReverseListCode)

  val mapReverseListCurryCode =
    """
      |import Standard.Base.Data.List.List
      |import Standard.Base.Data.Numbers
      |
      |List.mapReverse self f acc = case self of
      |    List.Cons h t -> @Tail_Call t.mapReverse f (List.Cons (f h) acc)
      |    _ -> acc
      |
      |main = list ->
      |    adder = x -> y -> x + y
      |    res = list.mapReverse (adder 1) List.Nil
      |    res
      |""".stripMargin
  val mapReverseListCurry = getMain(mapReverseListCurryCode)
}
