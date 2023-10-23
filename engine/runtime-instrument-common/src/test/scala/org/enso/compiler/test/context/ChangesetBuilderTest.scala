package org.enso.compiler.test.context

import org.enso.compiler.Passes
import org.enso.compiler.context.{
  ChangesetBuilder,
  FreshNameSupply,
  InlineContext,
  ModuleContext
}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{CallArgument, Expression, Function}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.pass.PassManager
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.text.buffer.Rope
import org.enso.text.editing.JavaEditorAdapter
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
        .asInstanceOf[Expression.Binding]
      val rhs = ir.expression.asInstanceOf[Application.Prefix]
      val two = rhs.arguments(1).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Expression.Binding]
      val rhs = ir.expression.asInstanceOf[Application.Prefix]
      val two = rhs.arguments(1).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Expression.Binding]
      val rhs = ir.expression.asInstanceOf[Application.Prefix]
      val two = rhs.arguments(1).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Expression.Binding]
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
        .asInstanceOf[Expression.Binding]
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
        .asInstanceOf[Expression.Binding]
      val rhs  = ir.expression.asInstanceOf[Application.Prefix]
      val plus = rhs.function
      val two  = rhs.arguments(1).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Expression.Binding]
      val rhs = ir.expression.asInstanceOf[Application.Prefix]
      val x   = ir.name
      val one = rhs.arguments(0).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Expression.Binding]
      val x   = ir.name
      val rhs = ir.expression.asInstanceOf[Application.Prefix]
      val one = rhs.arguments(0).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Expression.Binding]
      val undefinedExpr = ir.expression.asInstanceOf[errors.Resolution]
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
        .asInstanceOf[Expression.Binding]
      val rhs = ir.expression.asInstanceOf[Application.Prefix]
      val undefinedArg =
        rhs.arguments(1).asInstanceOf[CallArgument.Specified]
      val undefinedError = undefinedArg.value.asInstanceOf[errors.Resolution]
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
        .asInstanceOf[Function.Lambda]
      val secondLine =
        ir.body.children(1).asInstanceOf[Application.Prefix]
      val y =
        secondLine.arguments(0).asInstanceOf[CallArgument.Specified].value
      val plus = secondLine.function
      val x =
        secondLine.arguments(1).asInstanceOf[CallArgument.Specified].value

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
        .asInstanceOf[Function.Lambda]

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
        .asInstanceOf[Function.Lambda]
      val firstLine = ir.body.children(0).asInstanceOf[Expression.Binding]
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
        .asInstanceOf[Function.Lambda]
      val secondLine = ir.body.children(1).asInstanceOf[Expression.Binding]
      val z          = secondLine.expression.asInstanceOf[Application.Force].target
      val thirdLine =
        ir.body.children(2).asInstanceOf[Application.Prefix]
      val y =
        thirdLine.arguments(0).asInstanceOf[CallArgument.Specified].value
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
        .asInstanceOf[Expression.Binding]
      val x    = ir.name
      val rhs  = ir.expression.asInstanceOf[Application.Prefix]
      val one  = rhs.arguments(0).asInstanceOf[CallArgument.Specified].value
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
        .asInstanceOf[Expression.Binding]
      val body       = ir.expression.asInstanceOf[Function.Lambda].body
      val secondLine = body.children(1).asInstanceOf[Expression.Binding]
      val z          = secondLine.expression.asInstanceOf[Application.Force].target
      val thirdLine  = body.children(2).asInstanceOf[Application.Prefix]
      val y =
        thirdLine.arguments(0).asInstanceOf[CallArgument.Specified].value
      val plus = thirdLine.function

      invalidated(ir, code, edits: _*) should contain theSameElementsAs Seq(
        ir.name.getId,
        z.getId,
        y.getId,
        plus.getId
      )
    }

    "multiple last line" in {
      val code =
        """foo x =
          |    z = 1
          |    y = z
          |    y + x""".stripMargin.linesIterator.mkString("\n")
      val edits = Seq(
        TextEdit(Range(Position(3, 4), Position(3, 9)), "y + x + y"),
        TextEdit(Range(Position(3, 4), Position(3, 13)), "y + x + y + x"),
        TextEdit(Range(Position(3, 4), Position(3, 17)), "y + x + y + x + 1")
      )
      val source = Rope(code)
      val result = JavaEditorAdapter.applyEdits(source, edits)
      result.isRight shouldBe true
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
        ir.bindings(0).asInstanceOf[definition.Method.Explicit]
      val mainBody = main.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
      val x     = mainBody.expressions(0).asInstanceOf[Expression.Binding]
      val xExpr = x.expression.asInstanceOf[Application.Prefix]
      val undefinedName = xExpr
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[errors.Resolution]
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

    "toggle defaulted boolean parameter" in {
      implicit val moduleContext: ModuleContext = freshModuleContext

      val code =
        """
          |from Standard.Base import all
          |from Standard.Base.Data.Boolean import Boolean
          |import Standard.Visualization
          |
          |main =
          |    text2 = "  B   B   B   "
          |    text1 = "B"
          |    text3 = "@"
          |    operator2 = text2.replace text1 text3 (Case_Sensitivity.Insensitive)
          |
          |
          |
          |
          |#### METADATA ####
          |[[{"index":{"value":5},"size":{"value":8}},"6d97d061-2c1d-4540-8f7d-3d8aa765323d"],[{"index":{"value":13},"size":{"value":1}},"a6a1b572-4867-4317-8a6e-b496b286ea2d"],[{"index":{"value":14},"size":{"value":4}},"b82e52c6-e2f0-4e16-b835-59ab8838cb2c"],[{"index":{"value":5},"size":{"value":13}},"4dd74fbb-c6df-4109-8821-f4643d483355"],[{"index":{"value":0},"size":{"value":29}},"82014253-a4f5-431b-971b-2348f5a2ca1f"],[{"index":{"value":35},"size":{"value":8}},"b22aad40-a237-4d47-a140-930d731ca80e"],[{"index":{"value":43},"size":{"value":1}},"9d7f45f1-f537-480d-b65f-37a90c77455a"],[{"index":{"value":44},"size":{"value":4}},"c6ad377f-1fce-431d-bbd7-cfe88624c50d"],[{"index":{"value":35},"size":{"value":13}},"202d7fa4-c782-4bb8-900f-7bf5f9ebad99"],[{"index":{"value":48},"size":{"value":1}},"6847cb14-aa49-455d-be9d-e82a39460005"],[{"index":{"value":49},"size":{"value":4}},"36dcdde8-1667-495a-ab87-7154c8d3be67"],[{"index":{"value":35},"size":{"value":18}},"d67c4ce2-711a-4dd9-a1f8-b48f2730b141"],[{"index":{"value":53},"size":{"value":1}},"295fa6b1-260d-4f23-8bd6-64a1d60f01e5"],[{"index":{"value":54},"size":{"value":7}},"923a2769-ddc0-4339-b919-941155a4d1b7"],[{"index":{"value":35},"size":{"value":26}},"64ef6c8a-810e-42f4-8334-8b12c4ab14bc"],[{"index":{"value":69},"size":{"value":7}},"e8cf68da-2b41-4c70-b163-567a223459fd"],[{"index":{"value":30},"size":{"value":46}},"0b934140-6464-4a05-ae6c-416670e2c7e2"],[{"index":{"value":84},"size":{"value":8}},"eaa41303-7d43-420f-8d64-da8d376fab65"],[{"index":{"value":92},"size":{"value":1}},"d582d163-0d4b-4406-96a3-e4591766406d"],[{"index":{"value":93},"size":{"value":13}},"2649f663-b1b8-44a1-9158-f2cf23038471"],[{"index":{"value":84},"size":{"value":22}},"5b4e8628-e0b7-408e-8070-e5636514c7ac"],[{"index":{"value":77},"size":{"value":29}},"78558eb9-cfb0-41d1-8f9a-0f0c17db3b7c"],[{"index":{"value":108},"size":{"value":4}},"58c9224e-644d-40c1-be25-068b6b6dc947"],[{"index":{"value":113},"size":{"value":1}},"d01b8ec7-e780-40a3-b401-97e89e000484"],[{"index":{"value":120},"size":{"value":5}},"aa43682d-01ee-453d-9bcb-34c905d8d314"],[{"index":{"value":126},"size":{"value":1}},"2b5d6c3f-e030-4ee4-9102-b573f066efa0"],[{"index":{"value":128},"size":{"value":16}},"54493640-4981-4437-ad6e-fc80ab1b581f"],[{"index":{"value":120},"size":{"value":24}},"c04888d6-bc60-4534-aa7e-df3dd5d52410"],[{"index":{"value":149},"size":{"value":5}},"85ae1943-cfcf-4e53-97c7-f73fdae42529"],[{"index":{"value":155},"size":{"value":1}},"113ae47b-3a1d-497e-8a2a-b7775be6c66e"],[{"index":{"value":157},"size":{"value":3}},"c143e355-9d64-4780-b387-dd364eefa2f9"],[{"index":{"value":149},"size":{"value":11}},"73113bd8-d3cb-4b06-a4d7-425a88bca849"],[{"index":{"value":165},"size":{"value":5}},"a286047b-54a5-45bb-995e-60f207e2af65"],[{"index":{"value":171},"size":{"value":1}},"abace020-1a84-4fa4-a5ee-2f98f73dfcb1"],[{"index":{"value":173},"size":{"value":3}},"ed3e40fb-d19e-4fb6-bebd-f74ee179ae08"],[{"index":{"value":165},"size":{"value":11}},"8ac6594b-c472-41d5-9b00-f13e6f30d5f2"],[{"index":{"value":181},"size":{"value":9}},"25eaa42e-026d-4afd-a433-dde5804b7a5c"],[{"index":{"value":191},"size":{"value":1}},"799a83db-5893-40f7-9f1e-e07c7a3071cd"],[{"index":{"value":193},"size":{"value":5}},"49332623-fe9a-4fe5-9657-7c0ea2b23942"],[{"index":{"value":198},"size":{"value":1}},"7690e4c2-31e7-4a12-8f5c-78c4484b9553"],[{"index":{"value":199},"size":{"value":7}},"3606a45e-50be-49c5-9dda-415263145a15"],[{"index":{"value":193},"size":{"value":13}},"91640cfc-f985-49da-95c8-87181d725299"],[{"index":{"value":207},"size":{"value":5}},"afb24ea2-2f35-4c7e-8394-3ba34626484e"],[{"index":{"value":193},"size":{"value":19}},"86eb77c6-14cc-487a-8993-bf9998244321"],[{"index":{"value":213},"size":{"value":5}},"52a3e9b8-f8d0-43c9-9f6d-9785359c7ba7"],[{"index":{"value":193},"size":{"value":25}},"1cddcb6a-95c8-445b-9a6f-267093b2609c"],[{"index":{"value":220},"size":{"value":16}},"df114ed3-b64d-4e74-ad60-b43a7147ec19"],[{"index":{"value":236},"size":{"value":1}},"b6a14995-bb54-4956-9abd-9b8cff898ad3"],[{"index":{"value":237},"size":{"value":11}},"fdf942d8-8064-4067-ae38-131b80fa6e0c"],[{"index":{"value":220},"size":{"value":28}},"b8df7e14-9004-4f61-8076-09354f8dbd36"],[{"index":{"value":193},"size":{"value":56}},"0d4b3071-3f7d-472a-bfa7-af118222894e"],[{"index":{"value":181},"size":{"value":68}},"d9ee4c00-74b3-4859-a252-6c9105085076"],[{"index":{"value":115},"size":{"value":135}},"3eba123a-ea77-4db5-89d8-800d35e102e7"],[{"index":{"value":108},"size":{"value":142}},"11d6a77c-3644-4a27-8e15-16808af51c19"]]
          |""".stripMargin.linesIterator.mkString("\n")
      val edit =
        TextEdit(Range(Position(9, 72), Position(9, 72)), " Boolean.True")

      val ir     = code.preprocessModule
      val atId   = "0d4b3071-3f7d-472a-bfa7-af118222894e"
      val at     = findIR(ir, atId)
      val atCode = findCode(code, at)
      atCode shouldBe "text2.replace text1 text3 (Case_Sensitivity.Insensitive)"

      val all = new ChangesetBuilder(Rope(code), ir).invalidated(Seq(edit))

      all
        .map(n => n.externalId.getOrElse(n.internalId))
        .map(findCode(code, ir, _)) should contain theSameElementsAs Seq(
        atCode
      )
    }
  }

  def findIR(ir: IR, uuid: String): IR = {
    val list = ir.preorder.filter(
      _.location
        .map(_.id.map(_.toString() == uuid).getOrElse(false))
        .getOrElse(false)
    )
    if (list.isEmpty) {
      null
    } else {
      list.head
    }
  }

  def findCode(code: String, at: IR): String = {
    val loc = at.location.get.location
    code.substring(loc.start, loc.end)
  }

  def findCode(code: String, ir: IR, uuid: UUID): String = {
    val at = findIR(ir, uuid.toString())
    if (at == null) {
      uuid.toString
    } else {
      findCode(code, at)
    }
  }

  def invalidated(ir: IR, code: String, edits: TextEdit*): Set[IR.Identifier] =
    new ChangesetBuilder(Rope(code), ir)
      .invalidated(edits)
      .map(n => n.externalId.getOrElse(n.internalId))

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
