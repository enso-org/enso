package org.enso.compiler.test.context

import org.enso.compiler.Passes
import org.enso.compiler.context.{
  ChangesetBuilder,
  FreshNameSupply,
  InlineContext,
  ModuleContext
}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Application
import org.enso.compiler.pass.PassManager
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.text.buffer.Rope
import org.enso.text.editing.model.{Position, Range, TextEdit}

import java.util.UUID

class ChangesetBuilderTest extends CompilerTest {

  implicit val passManager: PassManager = new Passes(defaultConfig).passManager

  "DiffChangeset" should {

    "single literal whole" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 8), Position(0, 9)), "42")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Prefix]
      val two = rhs.arguments(1).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        two.getId
      )
    }

    "single literal left" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 8), Position(0, 8)), "9")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Prefix]
      val two = rhs.arguments(1).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        two.getId
      )
    }

    "single literal right" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 9), Position(0, 9)), "9")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Prefix]
      val two = rhs.arguments(1).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        two.getId
      )
    }

    "single literal partial" in {
      val code = """x1 = 42"""
      val edit = TextEdit(Range(Position(0, 1), Position(0, 2)), "")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val x = ir.name

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        x.getId
      )
    }

    "single literal inside" in {
      val code = """baz = 42"""
      val edit = TextEdit(Range(Position(0, 1), Position(0, 2)), "oo")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val x = ir.name

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        x.getId
      )
    }

    "application and literal" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 6), Position(0, 9)), "- 42")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val rhs  = ir.expression.asInstanceOf[IR.Application.Prefix]
      val plus = rhs.function
      val two  = rhs.arguments(1).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        plus.getId,
        two.getId
      )
    }

    "binding and literal" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 0), Position(0, 5)), "y = 42")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Prefix]
      val x   = ir.name
      val one = rhs.arguments(0).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        x.getId,
        one.getId
      )
    }

    "binding and space" in {
      val code = """x = 1 + 2"""
      val edit = TextEdit(Range(Position(0, 0), Position(0, 4)), "y = ")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val x   = ir.name
      val rhs = ir.expression.asInstanceOf[IR.Application.Prefix]
      val one = rhs.arguments(0).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        x.getId,
        one.getId
      )
    }

    "undefined binding whole" in {
      val code = """baz = undefined"""
      val edit = TextEdit(Range(Position(0, 6), Position(0, 15)), "42")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val undefinedExpr = ir.expression.asInstanceOf[IR.Error.Resolution]
      val undefinedName = undefinedExpr.originalName

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        undefinedName.getId
      )
    }

    "single undefined literal whole" in {
      val code = """x = 1 + undefined"""
      val edit = TextEdit(Range(Position(0, 8), Position(0, 16)), "42")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val rhs = ir.expression.asInstanceOf[IR.Application.Prefix]
      val undefinedArg =
        rhs.arguments(1).asInstanceOf[IR.CallArgument.Specified]
      val undefinedError = undefinedArg.value.asInstanceOf[IR.Error.Resolution]
      val undefinedName  = undefinedError.originalName

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        undefinedName.getId
      )
    }

    "multiline single line" in {
      val code =
        """x ->
          |    y = 5
          |    y + x""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(2, 4), Position(2, 9)), "x")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Function.Lambda]
      val secondLine =
        ir.body.children(1).asInstanceOf[IR.Application.Prefix]
      val y =
        secondLine.arguments(0).asInstanceOf[IR.CallArgument.Specified].value
      val plus = secondLine.function
      val x =
        secondLine.arguments(1).asInstanceOf[IR.CallArgument.Specified].value

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        y.getId,
        plus.getId,
        x.getId
      )
    }

    "multiline insert line 1" in {
      val code =
        """x ->
          |    y = 5
          |    IO.println y""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(2, 0), Position(2, 0)), "    z = 42\n")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Function.Lambda]

      invalidated(ir, code, edit) should contain theSameElementsAs Seq()
    }

    "multiline insert line 2" in {
      val code =
        """x ->
          |    y = 5
          |    IO.println y""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(1, 9), Position(1, 9)), "\n    z = 7")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Function.Lambda]
      val firstLine = ir.body.children(0).asInstanceOf[IR.Expression.Binding]
      val five      = firstLine.expression

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        five.getId
      )
    }

    "multiline across lines" in {
      val code =
        """x ->
          |    z = 1
          |    y = z
          |    y + x""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(2, 8), Position(3, 7)), s"42\n    y -")

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Function.Lambda]
      val secondLine = ir.body.children(1).asInstanceOf[IR.Expression.Binding]
      val z          = secondLine.expression.asInstanceOf[IR.Application.Force].target
      val thirdLine =
        ir.body.children(2).asInstanceOf[IR.Application.Prefix]
      val y =
        thirdLine.arguments(0).asInstanceOf[IR.CallArgument.Specified].value
      val plus = thirdLine.function

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        z.getId,
        y.getId,
        plus.getId
      )
    }

    "multiple single expression" in {
      val code = """x = 1 + 2"""
      val edits = Seq(
        TextEdit(Range(Position(0, 0), Position(0, 0)), "inde"),
        TextEdit(Range(Position(0, 8), Position(0, 9)), "40"),
        TextEdit(Range(Position(0, 11), Position(0, 12)), "-"),
        TextEdit(Range(Position(0, 8), Position(0, 10)), "44")
      )

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val x    = ir.name
      val rhs  = ir.expression.asInstanceOf[IR.Application.Prefix]
      val one  = rhs.arguments(0).asInstanceOf[IR.CallArgument.Specified].value
      val plus = rhs.function

      invalidated(ir, code, edits: _*) should contain theSameElementsAs Seq(
        x.getId,
        one.getId,
        plus.getId
      )
    }

    "multiple multiline" in {
      val code =
        """foo x =
          |    z = 1
          |    y = z
          |    y + x""".stripMargin.linesIterator.mkString("\n")
      val edits = Seq(
        TextEdit(Range(Position(0, 0), Position(0, 0)), "bar = 123\n\n"),
        TextEdit(Range(Position(4, 8), Position(5, 7)), "42\n    y -")
      )

      val ir = code
        .preprocessExpression(freshInlineContext)
        .get
        .asInstanceOf[IR.Expression.Binding]
      val body       = ir.expression.asInstanceOf[IR.Function.Lambda].body
      val secondLine = body.children(1).asInstanceOf[IR.Expression.Binding]
      val z          = secondLine.expression.asInstanceOf[Application.Force].target
      val thirdLine  = body.children(2).asInstanceOf[IR.Application.Prefix]
      val y =
        thirdLine.arguments(0).asInstanceOf[IR.CallArgument.Specified].value
      val plus = thirdLine.function

      invalidated(ir, code, edits: _*) should contain theSameElementsAs Seq(
        ir.name.getId,
        z.getId,
        y.getId,
        plus.getId
      )
    }

    "module with undefined literal" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """main =
          |    x = 1 + undefined
          |    y = x - 1
          |    y
          |
          |
          |#### METADATA ####
          |[[{"index": {"value": 47}, "size": {"value": 1}}, "b95f644b-e877-4e33-b5da-11a65e01068e"],[{"index": {"value": 37}, "size": {"value": 5}}, "17edd47d-b546-4d57-a453-0529036b393f"],[{"index": {"value": 15}, "size": {"value": 13}}, "b1c393b2-67be-488b-b46d-2adba21bca6d"]]
          |[]
          |""".stripMargin.linesIterator.mkString("\n")
      val edit = TextEdit(Range(Position(1, 12), Position(1, 21)), "42")

      val ir = code.preprocessModule
      val main =
        ir.bindings(0).asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
      val mainBody = main.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
      val x     = mainBody.expressions(0).asInstanceOf[IR.Expression.Binding]
      val xExpr = x.expression.asInstanceOf[IR.Application.Prefix]
      val undefinedName = xExpr
        .arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
        .value
        .asInstanceOf[IR.Error.Resolution]
        .originalName

      invalidated(ir, code, edit) should contain theSameElementsAs Seq(
        undefinedName.getId
      )
      invalidatedAll(ir, code, edit) should contain theSameElementsAs Seq(
        UUID.fromString("b1c393b2-67be-488b-b46d-2adba21bca6d"),
        UUID.fromString("17edd47d-b546-4d57-a453-0529036b393f"),
        UUID.fromString("b95f644b-e877-4e33-b5da-11a65e01068e")
      )
    }

  }

  def invalidated(ir: IR, code: String, edits: TextEdit*): Set[IR.Identifier] =
    new ChangesetBuilder(Rope(code), ir).invalidated(edits).map(_.internalId)

  def invalidatedAll(
    ir: IR,
    code: String,
    edits: TextEdit*
  ): Set[IR.ExternalId] =
    new ChangesetBuilder(Rope(code), ir).compute(edits)

  def freshModuleContext: ModuleContext =
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))

  def freshInlineContext: InlineContext =
    buildInlineContext(
      localScope       = Some(LocalScope.root),
      freshNameSupply  = Some(new FreshNameSupply),
      isInTailPosition = Some(false)
    )
}
