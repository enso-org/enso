package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.ir.expression.{Application, Operator}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.pass.desugar.FunctionBinding
import org.enso.compiler.pass.resolve.{DocumentationComments, ModuleAnnotations}
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class FunctionBindingTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup    = passes.getPrecursors(FunctionBinding).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run method and function desugaring on an
    * [[Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any sugared function and method definitions
      *        desugared
      */
    def desugar(implicit moduleContext: ModuleContext): Module = {
      FunctionBinding.runModule(ir, moduleContext)
    }
  }

  /** Adds an extension method to run function desugaring on an arbitrary
    * expression.
    *
    * @param ir the expression to desugar
    */
  implicit class DesugarExpression(ir: Expression) {

    /** Runs desgaring on an expression.
      *
      * @param inlineContext the inline context in which the desugaring is
      *                      taking place
      * @return [[ir]], with any sugared function definitions desugared
      */
    def desugar(implicit inlineContext: InlineContext): Expression = {
      FunctionBinding.runExpression(ir, inlineContext)
    }
  }

  /** Creates a defaulted module context.
    *
    * @return a defaulted module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext()
  }

  /** Creates a defaulted inline context.
    *
    * @return a defaulted inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext()
  }

  // === The Tests ============================================================

  "Sugared method definitions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |Unit.foo ~a _ (c = 1) = a + c
        |""".stripMargin.preprocessModule.desugar

    "desugar to standard method definitions" in {
      ir.bindings.head shouldBe an[definition.Method.Explicit]
    }

    val explicitMethod =
      ir.bindings.head.asInstanceOf[definition.Method.Explicit]

    "have the function arguments in the body functions" in {
      val lambda1 = explicitMethod.body.asInstanceOf[Function.Lambda]
      val lambda2 = lambda1.body.asInstanceOf[Function.Lambda]
      val lambda3 = lambda2.body.asInstanceOf[Function.Lambda]
      val cArg =
        lambda3.arguments.head.asInstanceOf[DefinitionArgument.Specified]

      lambda1.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .suspended shouldEqual true
      lambda1.arguments.head.name.name shouldEqual "a"
      lambda2.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name shouldBe an[Name.Blank]
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
        .asInstanceOf[definition.Method.Explicit]
        .body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]

      body.expressions.head shouldBe an[Expression.Binding]
      val binding = body.expressions.head.asInstanceOf[Expression.Binding]

      binding.expression shouldBe an[Function.Lambda]
    }

    "desugar module-level methods" in {
      val ir =
        """
          |foo x =
          |    y -> x + y
          |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[definition.Method.Explicit]
    }
  }

  "Conversion method definitions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val from: String = FunctionBinding.conversionMethodName

    "be turned into Method.Conversion IR entities" in {
      val ir =
        s"""My_Type.$from (that : Other) ~config=Nothing = My_Type value.a
           |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[definition.Method.Conversion]
      val conversion = ir.bindings.head
        .asInstanceOf[definition.Method.Conversion]
      conversion.sourceTypeName.asInstanceOf[Name].name shouldEqual "Other"
      val arguments = conversion.body.asInstanceOf[Function.Lambda].arguments
      arguments.length shouldEqual 1
      arguments.head.name.name shouldEqual "that"
      arguments.head.ascribedType shouldBe defined
      arguments.head.defaultValue should not be defined
      arguments.head.suspended shouldBe false

      val subLambda = conversion.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
      val subArguments = subLambda.arguments
      subArguments.length shouldEqual 1
      subArguments.head.name.name shouldEqual "config"
      subArguments.head.ascribedType should not be defined
      subArguments.head.defaultValue shouldBe defined
      subArguments.head.suspended shouldBe true
    }

    "retain documentation comments and annotations associated with them" in {
      val ir =
        s"""## Module doc
           |
           |## My documentation for this conversion.
           |@My_Annotation
           |My_Type.$from (that : Value) = that
           |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[definition.Method.Conversion]
      val conversion = ir.bindings.head
        .asInstanceOf[definition.Method.Conversion]

      val annotations =
        conversion.unsafeGetMetadata(ModuleAnnotations, "Should be present.")
      annotations.annotations.length shouldEqual 1
      annotations.annotations.head.name shouldEqual "@My_Annotation"

      val doc = conversion.unsafeGetMetadata(
        DocumentationComments,
        "Should be present."
      )
      doc.documentation shouldEqual " My documentation for this conversion."
    }

    "return an error if the conversion has no arguments" in {
      val ir =
        s"""My_Type.$from = a + b
           |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[errors.Conversion]
      val err = ir.bindings.head.asInstanceOf[errors.Conversion]
      err.reason shouldBe an[errors.Conversion.MissingArgs.type]
    }

    "return an error if the conversion does not have a source type" in {
      val ir =
        s"""My_Type.$from that = that + that
           |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[errors.Conversion]
      val err = ir.bindings.head.asInstanceOf[errors.Conversion]
      err.reason shouldBe an[errors.Conversion.MissingSourceType]
    }

    "return an error if the additional arguments don't have defaults" in {
      val ir =
        s"""My_Type.$from (that : Other) config = that + that
           |""".stripMargin.preprocessModule.desugar

      ir.bindings.head shouldBe an[errors.Conversion]
      val err = ir.bindings.head.asInstanceOf[errors.Conversion]
      err.reason shouldBe an[errors.Conversion.NonDefaultedArgument]
    }

    "not return an error if the additional arguments don't have defaults and is not a self parameter" in {
      val ir =
        s"""My_Type.$from (that : Other) config=1 self = that + that
           |""".stripMargin.preprocessModule.desugar

      ir.bindings.head should not be an[Error]
    }
  }

  "Sugared function definitions" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """
        |f ~a _ (c = 1) = a + b * c
        |""".stripMargin.preprocessExpression.get.desugar

    "desugar to a binding with a lambda" in {
      ir shouldBe an[Expression.Binding]
      val binding = ir.asInstanceOf[Expression.Binding]

      binding.name.name shouldEqual "f"
      binding.expression shouldBe an[Function.Lambda]
    }

    "work properly for complex argument definition types" in {
      val lambda1 = ir
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Function.Lambda]
      val lambda2 = lambda1.body.asInstanceOf[Function.Lambda]
      val lambda3 = lambda2.body.asInstanceOf[Function.Lambda]
      val cArg =
        lambda3.arguments.head.asInstanceOf[DefinitionArgument.Specified]

      lambda1.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .suspended shouldEqual true
      lambda2.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name shouldBe an[Name.Blank]
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
          .asInstanceOf[Expression.Binding]

      val aArg = ir.expression
        .asInstanceOf[Function.Lambda]
        .arguments
        .head
        .asInstanceOf[DefinitionArgument.Specified]
      aArg.name.name shouldEqual "a"
      aArg.defaultValue.get
        .asInstanceOf[Operator.Binary]
        .left
        .value
        .asInstanceOf[Application.Prefix]
        .function
        .asInstanceOf[Name.Literal]
        .name shouldEqual "f"

      val body = ir.expression
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
      body.expressions.head shouldBe an[Expression.Binding]

      val gBinding = body.expressions.head.asInstanceOf[Expression.Binding]
      gBinding.name.name shouldEqual "g"
      gBinding.expression shouldBe an[Function.Lambda]
    }
  }
}
