package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.ir.Function
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.Literal
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.pass.resolve.ExpressionAnnotations
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ExpressionAnnotationsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  def mkInlineContext: InlineContext =
    buildInlineContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(ExpressionAnnotations).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext): Module = {
      ExpressionAnnotations.runModule(ir, context)
    }
  }

  implicit class AnalyseExpression(ir: Expression) {
    def analyse(implicit context: InlineContext): Expression = {
      ExpressionAnnotations.runExpression(ir, context)
    }
  }

  // === The Tests ============================================================

  "Annotations resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |foo x =
        |    first expression
        |    @Unknown_Annotation foo bar baz
        |    @Builtin_Method "myBuiltin"
        |    @Auto_Parallel a.f bar baz
        |    foo @Tail_Call
        |    foo (@Tail_Call bar baz)
        |""".stripMargin.preprocessModule.analyse

    val items = ir.bindings.head
      .asInstanceOf[definition.Method.Explicit]
      .body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]

    "create an error when discovering an unknown annotation" in {
      val unknown =
        items.expressions(1).asInstanceOf[Application.Prefix].function
      unknown shouldBe an[errors.Resolution]
      unknown
        .asInstanceOf[errors.Resolution]
        .reason shouldEqual errors.Resolution.UnknownAnnotation
    }

    "associate the annotation with the annotated definition" in {
      val builtinDef = items.expressions(2).asInstanceOf[Literal.Text]
      builtinDef.text shouldEqual "myBuiltin"
      builtinDef
        .unsafeGetMetadata(ExpressionAnnotations, "")
        .annotations
        .head
        .name shouldEqual ExpressionAnnotations.builtinMethodName

      val parallelDef = items.expressions(3).asInstanceOf[Application.Prefix]
      parallelDef.function shouldBe a[Name.Literal]
      val fn = parallelDef.function.asInstanceOf[Name.Literal]
      fn.name shouldEqual "f"
      fn.isMethod shouldBe true
      parallelDef
        .unsafeGetMetadata(ExpressionAnnotations, "")
        .annotations
        .head
        .name shouldEqual ExpressionAnnotations.autoParallelName

      val correct = items.returnValue
        .asInstanceOf[Application.Prefix]
        .arguments(0)
        .value
        .asInstanceOf[Application.Prefix]
      correct.function.asInstanceOf[Name].name shouldEqual "bar"
      correct.arguments.length shouldEqual 1
      correct
        .getMetadata(ExpressionAnnotations)
        .get
        .annotations
        .head
        .name shouldEqual "@Tail_Call"
    }

    "create an error on a misplaced annotation" in {
      val misplaced = items
        .expressions(4)
        .asInstanceOf[Application.Prefix]
        .arguments(0)
        .value
      misplaced shouldBe an[errors.Resolution]
      misplaced
        .asInstanceOf[errors.Resolution]
        .reason shouldEqual errors.Resolution.UnexpectedAnnotation
    }
  }

  "Annotations resolution 2" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |foo x =
        |    @Tail_Call
        |""".stripMargin.preprocessModule.analyse

    val items = ir.bindings.head
      .asInstanceOf[definition.Method.Explicit]
      .body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]

    "create an error when discovering an unexpected annotation" in {
      items.expressions.size shouldBe 0
      items.returnValue shouldBe an[errors.Resolution]
      val err = items.returnValue.asInstanceOf[errors.Resolution]
      err
        .asInstanceOf[errors.Resolution]
        .reason shouldEqual errors.Resolution.UnexpectedAnnotation
    }
  }
}
