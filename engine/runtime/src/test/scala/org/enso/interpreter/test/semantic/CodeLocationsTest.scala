package org.enso.interpreter.test.semantic
import org.enso.interpreter.node.callable.function.CreateFunctionNode
import org.enso.interpreter.node.callable.thunk.ForceNode
import org.enso.interpreter.node.callable.ApplicationNode
import org.enso.interpreter.node.controlflow.caseexpr.CaseNode
import org.enso.interpreter.node.expression.literal.LiteralNode
import org.enso.interpreter.node.scope.{AssignmentNode, ReadLocalVariableNode}
import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.polyglot.MethodNames

class CodeLocationsTest extends InterpreterTest {

  override def subject: String = "Code Locations"

  def debugPrintCodeLocations(code: String): Unit = {
    var off = 0
    code.linesIterator.toList.foreach { line =>
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

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "be correct in simple arithmetic expressions" in
    withLocationsInstrumenter { instrumenter =>
      val code = "main = 2 + 45 * 20"
      instrumenter.assertNodeExists(7, 11, classOf[ApplicationNode])
      instrumenter.assertNodeExists(11, 7, classOf[ApplicationNode])
      instrumenter.assertNodeExists(11, 2, classOf[LiteralNode])
      eval(code)
      ()
    }

    "be correct with parenthesized expressions" in
    withLocationsInstrumenter { instrumenter =>
      val code = "main = (2 + 45) * 20"
      instrumenter.assertNodeExists(7, 13, classOf[ApplicationNode])
      instrumenter.assertNodeExists(8, 6, classOf[ApplicationNode])
      eval(code)
      ()
    }

    "be correct in applications and method calls" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """from Standard.Base.Data.List import Cons
          |
          |main = (2-2 == 0).if_then_else (Cons 5 6) 0
          |""".stripMargin
      instrumenter.assertNodeExists(49, 36, classOf[ApplicationNode])
      instrumenter.assertNodeExists(74, 8, classOf[ApplicationNode])
      eval(code)
      ()
    }

    "be correct in assignments and variable reads" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |import Standard.Base.IO
          |
          |main =
          |    x = 2 + 2 * 2
          |    y = x * x
          |    IO.println y
          |""".stripMargin
      instrumenter.assertNodeExists(37, 13, classOf[AssignmentNode])
      instrumenter.assertNodeExists(55, 9, classOf[AssignmentNode])
      instrumenter.assertNodeExists(59, 1, classOf[ReadLocalVariableNode])
      instrumenter.assertNodeExists(63, 1, classOf[ReadLocalVariableNode])
      instrumenter.assertNodeExists(80, 1, classOf[ReadLocalVariableNode])
      eval(code)
      ()
    }

    "be correct for deeply nested functions" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |import Standard.Base.Nothing
          |import Standard.Base.IO
          |
          |Nothing.method =
          |    foo = a -> b ->
          |        IO.println a
          |        add = a -> b -> a + b
          |        add a b
          |    foo 10 20
          |
          |main = Nothing.method
          |""".stripMargin

      instrumenter.assertNodeExists(137, 5, classOf[ApplicationNode])
      instrumenter.assertNodeExists(155, 1, classOf[ReadLocalVariableNode])
      instrumenter.assertNodeExists(151, 7, classOf[ApplicationNode])
      instrumenter.assertNodeExists(163, 9, classOf[ApplicationNode])
      eval(code)
      ()
    }

    "be correct inside pattern matches" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |from Standard.Base.Data.List import all
          |
          |main =
          |    x = Cons 1 2
          |    y = Nil
          |
          |    add = a -> b -> a + b
          |
          |    foo = x -> case x of
          |        Cons a b ->
          |            z = add a b
          |            x = z * z
          |            x
          |        _ -> 5 * 5
          |
          |    foo x + foo y
          |""".stripMargin
      instrumenter.assertNodeExists(121, 0, 109, 1, classOf[CaseNode])
      instrumenter.assertNodeExists(167, 7, classOf[ApplicationNode])
      instrumenter.assertNodeExists(187, 9, classOf[AssignmentNode])
      instrumenter.assertNodeExists(224, 5, classOf[ApplicationNode])
      eval(code)
      ()
    }

    "be correct for lambdas" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    f = a -> b -> a + b
          |    g = x -> y ->
          |        z = x * y
          |        z + z
          |
          |    f 1 (g 2 3)
          |""".stripMargin
      instrumenter.assertNodeExists(16, 15, classOf[CreateFunctionNode])
      instrumenter.assertNodeExists(40, 42, classOf[CreateFunctionNode])
      eval(code)
      ()
    }

    "be correct for defaulted arguments" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    bar = x -> x + x * x
          |    foo = x -> (y = bar x) -> x + y
          |    foo 0
          |""".stripMargin

      instrumenter.assertNodeExists(53, 5, classOf[ApplicationNode])
      instrumenter.assertNodeExists(53, 3, classOf[ReadLocalVariableNode])
      instrumenter.assertNodeExists(57, 1, classOf[ReadLocalVariableNode])
      eval(code)
      ()
    }

    "be correct for lazy arguments" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    bar = a -> ~b -> ~c -> b
          |
          |    bar 0 10 0
          |""".stripMargin
      instrumenter.assertNodeExists(35, 1, classOf[ForceNode])
      eval(code)
      ()
    }

    "be correct for negated literals" in
    withLocationsInstrumenter { instrumenter =>
      val code = "main = (-1)"
      instrumenter.assertNodeExists(7, 1, 4, 2, classOf[LiteralNode])
      eval(code)
    }

    "be correct for negated expressions" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |main =
          |    f = 1
          |    -f
          |""".stripMargin
      instrumenter.assertNodeExists(22, 1, 2, 1, classOf[ApplicationNode])
      eval(code)
    }

    "be correct in sugared method definitions" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |Test.foo a b = a * b - a
          |
          |main = Test.foo 2 3
          |""".stripMargin

      val mod    = interpreterContext.executionContext.evalModule(code, "Test")
      val tpe    = mod.getAssociatedType
      val method = mod.getMethod(tpe, "foo").get
      method.value.invokeMember(
        MethodNames.Function.GET_SOURCE_START
      ) shouldEqual 1
      method.value.invokeMember(
        MethodNames.Function.GET_SOURCE_LENGTH
      ) should (
        equal(24) or
        equal(25)
      )

      instrumenter.assertNodeExists(16, 9, classOf[ApplicationNode])

      eval(code)
    }

    "be correct in sugared function definitions" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """|main =
           |    f a b = a - b
           |    f 10 20
           |""".stripMargin

      instrumenter.assertNodeExists(11, 1, 13, 0, classOf[AssignmentNode])
      instrumenter.assertNodeExists(19, 1, 5, 0, classOf[ApplicationNode])
      eval(code)
    }

    "be correct in the presence of comments" in
    withLocationsInstrumenter { instrumenter =>
      val code =
        """
          |# this is a comment
          |#this too
          |## But this is a doc.
          |main = # define main
          |    y = 1 # assign one to `y`
          |    x = 2 # assign two to #x
          |    # perform the addition
          |    x + y # the addition is performed here
          |""".stripMargin.linesIterator.mkString("\n")
      instrumenter.assertNodeExists(82, 1, classOf[LiteralNode])
      instrumenter.assertNodeExists(164, 5, classOf[ApplicationNode])
      eval(code) shouldEqual 3
    }

    "be correct in nested pattern matches" in withLocationsInstrumenter {
      instrumenter =>
        val code =
          """
            |from Standard.Base.Data.List import all
            |
            |type MyAtom
            |
            |main =
            |    f = case _ of
            |        Cons (Cons MyAtom Nil) Nil -> 100
            |        _ -> 50
            |    f (Cons (Cons MyAtom Nil) Nil)
            |""".stripMargin

        instrumenter.assertNodeExists(70, 67, classOf[CaseNode])
        instrumenter.assertNodeExists(75, 1, classOf[ReadLocalVariableNode])
        instrumenter.assertNodeExists(118, 3, classOf[LiteralNode])

        eval(code) shouldEqual 100
    }
  }
}
