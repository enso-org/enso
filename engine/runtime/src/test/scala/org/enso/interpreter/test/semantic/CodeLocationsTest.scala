package org.enso.interpreter.test.semantic
import org.enso.interpreter.node.callable.ApplicationNode
import org.enso.interpreter.node.callable.function.CreateFunctionNode
import org.enso.interpreter.node.callable.thunk.ForceNode
import org.enso.interpreter.node.controlflow.MatchNode
import org.enso.interpreter.node.expression.literal.IntegerLiteralNode
import org.enso.interpreter.node.expression.operator.{AddOperatorNode, MultiplyOperatorNode}
import org.enso.interpreter.node.scope.{AssignmentNode, ReadLocalTargetNode}
import org.enso.interpreter.test.InterpreterTest

class CodeLocationsTest extends InterpreterTest {
  def debugPrintCodeLocations(code: String): Unit = {
    var off = 0
    code.lines.toList.foreach { line =>
      val chars: List[Any] = line.toList.map { c =>
          s" ${if (c == ' ') '_' else c} "
        } :+ '↵'
      val ixes = off.until(off + chars.length).map { i =>
        if (i.toString.length == 1) s" $i " else s"$i "
      }
      println(ixes.mkString(""))
      println(chars.mkString(""))
      println()
      off += line.length + 1
    }
  }

  "Code Locations" should "be correct in simple arithmetic expressions" in
  withLocationsInstrumenter { instrumenter =>
    val code = "2 + 45 * 20"
    instrumenter.assertNodeExists(0, 11, classOf[AddOperatorNode])
    instrumenter.assertNodeExists(4, 7, classOf[MultiplyOperatorNode])
    instrumenter.assertNodeExists(4, 2, classOf[IntegerLiteralNode])
    eval(code)
    ()
  }

  "Code locations" should "be correct with parenthesized expressions" in
  withLocationsInstrumenter { instrumenter =>
    val code = "(2 + 45) * 20"
    instrumenter.assertNodeExists(0, 13, classOf[MultiplyOperatorNode])
    instrumenter.assertNodeExists(1, 6, classOf[AddOperatorNode])
    eval(code)
    ()
  }

  "Code Locations" should "be correct in applications and method calls" in
  withLocationsInstrumenter { instrumenter =>
    val code = "(2 - 2).ifZero (Cons 5 6) 0"
    instrumenter.assertNodeExists(0, 27, classOf[ApplicationNode])
    instrumenter.assertNodeExists(16, 8, classOf[ApplicationNode])
    eval(code)
    ()
  }

  "Code Locations" should "be correct in assignments and variable reads" in
  withLocationsInstrumenter { instrumenter =>
    val code =
      """
        |x = 2 + 2 * 2
        |y = x * x
        |IO.println y
        |""".stripMargin
    instrumenter.assertNodeExists(1, 13, classOf[AssignmentNode])
    instrumenter.assertNodeExists(15, 9, classOf[AssignmentNode])
    instrumenter.assertNodeExists(19, 1, classOf[ReadLocalTargetNode])
    instrumenter.assertNodeExists(23, 1, classOf[ReadLocalTargetNode])
    instrumenter.assertNodeExists(36, 1, classOf[ReadLocalTargetNode])
    eval(code)
    ()
  }

  "Code Locations" should "be correct for deeply nested functions" in
  withLocationsInstrumenter { instrumenter =>
    val code =
      """
        |Unit.method =
        |  foo = a b ->
        |    IO.println a
        |    add = a -> b -> a + b
        |    add a b
        |  foo 10 20
        |
        |Unit.method
        |""".stripMargin
    instrumenter.assertNodeExists(67, 5, classOf[AddOperatorNode])
    instrumenter.assertNodeExists(81, 1, classOf[ReadLocalTargetNode])
    instrumenter.assertNodeExists(77, 7, classOf[ApplicationNode])
    instrumenter.assertNodeExists(87, 9, classOf[ApplicationNode])
    eval(code)
    ()
  }

  "Code Locations" should "be correct inside pattern matches" in
  withLocationsInstrumenter { instrumenter =>
    val code =
      """
        |x = Cons 1 2
        |y = Nil
        |
        |add = a b -> a + b
        |
        |foo = x -> case x of
        |  Cons a b ->
        |    z = add a b
        |    x = z * z
        |    x
        |  _ -> 5 * 5
        |
        |foo x + foo y
        |""".stripMargin
    instrumenter.assertNodeExists(54, 73, classOf[MatchNode])
    instrumenter.assertNodeExists(86, 7, classOf[ApplicationNode])
    instrumenter.assertNodeExists(98, 9, classOf[AssignmentNode])
    instrumenter.assertNodeExists(121, 5, classOf[MultiplyOperatorNode])
    eval(code)
    ()
  }

  "Code locations" should "be correct for lambdas" in
  withLocationsInstrumenter { instrumenter =>
    val code =
      """
        |f = a b -> a + b
        |g = x y ->
        |  z = x * y
        |  z + z
        |
        |f 1 (g 2 3)
        |""".stripMargin
    instrumenter.assertNodeExists(5, 12, classOf[CreateFunctionNode])
    instrumenter.assertNodeExists(22, 27, classOf[CreateFunctionNode])
    eval(code)
    ()
  }

  "Code locations" should "be correct for defaulted arguments" in
  withLocationsInstrumenter { instrumenter =>
    val code =
      """
        |bar = x -> x + x * x
        |foo = x (y = bar x) -> x + y
        |foo 0
        |""".stripMargin
    instrumenter.assertNodeExists(35, 5, classOf[ApplicationNode])
    instrumenter.assertNodeExists(35, 3, classOf[ReadLocalTargetNode])
    instrumenter.assertNodeExists(39, 1, classOf[ReadLocalTargetNode])
    eval(code)
    ()
  }

  "Code locations" should "be correct for lazy arguments" in
  withLocationsInstrumenter { instrumenter =>
    val code =
      """
        |bar = a ~b ~c -> a + ~b + ~c
        |
        |bar 0 10 0
        |""".stripMargin
    instrumenter.assertNodeExists(22, 2, classOf[ForceNode])
    eval(code)
    ()
  }
}
