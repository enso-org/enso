package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.desugar.FunctionBinding
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest

class FunctionBindingTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: List[IRPass] = passes.getPrecursors(FunctionBinding).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(precursorPasses, passConfig)

  /** Adds an extension method to run method and function desugaring on an
    * [[IR.Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: IR.Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any sugared function and method definitions
      *        desugared
      */
    def desugar(implicit moduleContext: ModuleContext): IR.Module = {
      FunctionBinding.runModule(ir, moduleContext)
    }
  }

  /** Adds an extension method to run function desugaring on an arbitrary
    * expression.
    *
    * @param ir the expression to desugar
    */
  implicit class DesugarExpression(ir: IR.Expression) {

    /** Runs desgaring on an expression.
      *
      * @param inlineContext the inline context in which the desugaring is
      *                      taking place
      * @return [[ir]], with any sugared function definitions desugared
      */
    def desugar(implicit inlineContext: InlineContext): IR.Expression = {
      FunctionBinding.runExpression(ir, inlineContext)
    }
  }

  /** Creates a defaulted module context.
    *
    * @return a defaulted module context
    */
  def mkModuleContext: ModuleContext = {
    ModuleContext()
  }

  /** Creates a defaulted inline context.
    *
    * @return a defaulted inline context
    */
  def mkInlineContext: InlineContext = {
    InlineContext()
  }

  // === The Tests ============================================================

  "Sugared method definitions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |Unit.foo ~a _ (c = 1) = a + c
        |""".stripMargin.preprocessModule.desugar

    "desugar to standard method definitions" in {
      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Method.Explicit]
    }

    val explicitMethod =
      ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]

    "have the function arguments in the body functions" in {
      val lambda1 = explicitMethod.body.asInstanceOf[IR.Function.Lambda]
      val lambda2 = lambda1.body.asInstanceOf[IR.Function.Lambda]
      val lambda3 = lambda2.body.asInstanceOf[IR.Function.Lambda]
      val cArg =
        lambda3.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]

      lambda1.arguments.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .suspended shouldEqual true
      lambda1.arguments.head.name.name shouldEqual "a"
      lambda2.arguments.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .name shouldBe an[IR.Name.Blank]
      cArg.name.name shouldEqual "c"
      cArg.defaultValue shouldBe defined
    }

    "desugar nested sugared functions" in {
      val ir =
        """
          |Foo.bar a =
          |    f b = b
          |    f 1
          |""".stripMargin.preprocessModule.desugar

      val body = ir.bindings.head
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]

      body.expressions.head shouldBe an[IR.Expression.Binding]
      val binding = body.expressions.head.asInstanceOf[IR.Expression.Binding]

      binding.expression shouldBe an[IR.Function.Lambda]
    }

    "desugar module-level methods" in {
      val ir =
        """
          |foo x =
          |    y -> x + y
          |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[IR.Module.Scope.Definition.Method.Explicit]
    }
  }

  "Sugared function definitions" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """
        |f ~a _ (c = 1) = a + b * c
        |""".stripMargin.preprocessExpression.get.desugar

    "desugar to a binding with a lambda" in {
      ir shouldBe an[IR.Expression.Binding]
      val binding = ir.asInstanceOf[IR.Expression.Binding]

      binding.name.name shouldEqual "f"
      binding.expression shouldBe an[IR.Function.Lambda]
    }

    "work properly for complex argument definition types" in {
      val lambda1 = ir
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Function.Lambda]
      val lambda2 = lambda1.body.asInstanceOf[IR.Function.Lambda]
      val lambda3 = lambda2.body.asInstanceOf[IR.Function.Lambda]
      val cArg =
        lambda3.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]

      lambda1.arguments.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .suspended shouldEqual true
      lambda2.arguments.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .name shouldBe an[IR.Name.Blank]
      cArg.name.name shouldEqual "c"
      cArg.defaultValue shouldBe defined
    }

    "work recursively" in {
      val ir =
        """
          |f (a = (f a = a)) =
          |    g b = b
          |    g 1
          |""".stripMargin.preprocessExpression.get.desugar
          .asInstanceOf[IR.Expression.Binding]

      val aArg = ir.expression
        .asInstanceOf[IR.Function.Lambda]
        .arguments
        .head
        .asInstanceOf[IR.DefinitionArgument.Specified]
      aArg.name.name shouldEqual "a"
      aArg.defaultValue.get
        .asInstanceOf[IR.Expression.Binding]
        .name
        .name shouldEqual "f"

      val body = ir.expression
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
      body.expressions.head shouldBe an[IR.Expression.Binding]

      val gBinding = body.expressions.head.asInstanceOf[IR.Expression.Binding]
      gBinding.name.name shouldEqual "g"
      gBinding.expression shouldBe an[IR.Function.Lambda]
    }
  }
}
