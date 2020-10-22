package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.resolve.Annotations
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class AnnotationsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes

  val precursorPasses: PassGroup =
    passes.getPrecursors(Annotations).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: IR.Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext): IR.Module = {
      Annotations.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Annotations resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |foo x =
        |    @Tail_Call
        |    @Unknown_Annotation foo bar baz
        |    foo @Tail_Call
        |    foo (@Tail_Call bar baz)
        |""".stripMargin.preprocessModule.analyse

    "resolve and mark annotations" in {
      val items = ir.bindings.head
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
      items.expressions(0) shouldBe an[IR.Error.Resolution]
      items
        .expressions(0)
        .asInstanceOf[IR.Error.Resolution]
        .reason shouldEqual IR.Error.Resolution.UnexpectedTailCallAnnotation

      val unknown =
        items.expressions(1).asInstanceOf[IR.Application.Prefix].function
      unknown shouldBe an[IR.Error.Resolution]
      unknown
        .asInstanceOf[IR.Error.Resolution]
        .reason shouldEqual IR.Error.Resolution.UnknownAnnotation

      val misplaced = items
        .expressions(2)
        .asInstanceOf[IR.Application.Prefix]
        .arguments(0)
        .value
      misplaced shouldBe an[IR.Error.Resolution]
      misplaced
        .asInstanceOf[IR.Error.Resolution]
        .reason shouldEqual IR.Error.Resolution.UnexpectedTailCallAnnotation

      val correct = items.returnValue
        .asInstanceOf[IR.Application.Prefix]
        .arguments(0)
        .value
        .asInstanceOf[IR.Application.Prefix]
      correct.function.asInstanceOf[IR.Name].name shouldEqual "bar"
      correct.arguments.length shouldEqual 1
      correct.getMetadata(Annotations) should contain(
        Annotations.TailCallAnnotated
      )
    }
  }
}
