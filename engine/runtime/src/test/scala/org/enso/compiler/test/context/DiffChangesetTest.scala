package org.enso.compiler.test.context

import org.enso.compiler.context.DiffChangeset
import org.enso.compiler.core.IR
import org.enso.compiler.test.CompilerTest
import org.enso.text.editing.model.{Position, Range, TextEdit}

class DiffChangesetTest extends CompilerTest {

  val dc = new DiffChangeset

  "DiffChangeset" should {

    "single literal whole" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 8), Position(0, 9)), "42")

      val ir  = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Operator.Binary]
      val two = rhs.right.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        two.getId
      )
    }

    "single literal left" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 8), Position(0, 8)), "9")

      val ir  = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Operator.Binary]
      val two = rhs.right.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        two.getId
      )
    }

    "single literal right" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 9), Position(0, 9)), "9")

      val ir  = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Operator.Binary]
      val two = rhs.right.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        two.getId
      )
    }

    "application and literal" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 6), Position(0, 9)), "- 42")

      val ir   = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val rhs  = ir.expression.asInstanceOf[IR.Application.Operator.Binary]
      val plus = rhs.operator
      val two  = rhs.right.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        plus.getId,
        two.getId
      )
    }

    "binding and literal" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 0), Position(0, 5)), "y = 42")

      val ir  = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Operator.Binary]
      val x   = ir.name
      val one = rhs.left.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        x.getId,
        one.getId
      )
    }

    "binding and space" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 0), Position(0, 4)), "y = ")

      val ir  = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val x   = ir.name
      val one =
        ir.expression.asInstanceOf[IR.Application.Operator.Binary].left.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        x.getId,
        one.getId
      )
    }

    "partial" in {
      val code = """x1 = 42"""
      val edit = TextEdit(Range(Position(0, 1), Position(0, 2)), "")

      val ir = code.toIrExpression.get.asInstanceOf[IR.Expression.Binding]
      val x  = ir.name

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        x.getId
      )
    }

    "multiline single line" in {
      val code =
        """x ->
          |    y = 5
          |    y + x""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(2, 4), Position(2, 9)), "x")

      val ir = code.toIrExpression.get.asInstanceOf[IR.Function.Lambda]
      val secondLine =
        ir.body.children(1).asInstanceOf[IR.Application.Operator.Binary]
      val y    = secondLine.left.value
      val plus = secondLine.operator
      val x    = secondLine.right.value

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        y.getId,
        plus.getId,
        x.getId
      )
    }

    "multiline across lines" in {
      val code =
        """x ->
          |    y = 5
          |    y + x""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(1, 8), Position(2, 7)), "42\n    y -")

      val ir        = code.toIrExpression.get.asInstanceOf[IR.Function.Lambda]
      val firstLine = ir.body.children(0).asInstanceOf[IR.Expression.Binding]
      val five      = firstLine.expression
      val secondLine =
        ir.body.children(1).asInstanceOf[IR.Application.Operator.Binary]
      val y    = secondLine.left.value
      val plus = secondLine.operator

      dc.compute(edit, code, ir) should contain theSameElementsAs Seq(
        five.getId,
        y.getId,
        plus.getId
      )
    }
  }
}
